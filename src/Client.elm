module Client exposing (main)

import AnyDict exposing (AnyDict)
import Browser exposing (Document)
import Browser.Dom
import Editor.Field exposing (Field)
import Editor.Theme
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Extra
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Random
import Result.Extra
import Task
import UUID exposing (UUID)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- INIT ----


type alias Model =
    { declarations : List ( UUID, EditorDeclaration )
    , expressions : Expressions
    , uuidSeeds : UUID.Seeds
    , mainId : UUID
    , resultOfRun : Result String String
    }


type alias Expressions =
    AnyDict String UUID EditorBlock


uuidDict : { toComparable : UUID -> String, fromComparable : String -> UUID }
uuidDict =
    { toComparable = UUID.toString
    , fromComparable = UUID.fromString >> Result.withDefault UUID.urlNamespace -- this should never happen
    }


type alias EditorDeclaration =
    { name : Field String
    , body : UUID
    }


type alias Flags =
    { seed1 : Int
    , seed2 : Int
    , seed3 : Int
    , seed4 : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( mainId, mainUuidSeeds ) =
            UUID.step
                { seed1 = Random.initialSeed flags.seed1
                , seed2 = Random.initialSeed flags.seed2
                , seed3 = Random.initialSeed flags.seed3
                , seed4 = Random.initialSeed flags.seed4
                }

        ( mainBodyId, finalUuidSeeds ) =
            UUID.step mainUuidSeeds
    in
    ( { declarations =
            [ ( mainId
              , { name = Editor.Field.fromString Ok "main"
                , body = mainBodyId
                }
              )
            ]
      , expressions =
            AnyDict.singleton uuidDict
                mainBodyId
                (newBlock (EditorExprUnknown ""))
      , uuidSeeds = finalUuidSeeds
      , mainId = mainId
      , resultOfRun = Err "not yet run"
      }
    , Cmd.none
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- UPDATE ----


type Msg
    = AddNewDeclaration Int String
    | SetDeclarationName UUID String
    | SetExpressionRaw UUID String
    | CalculateExpression UUID
    | NoOp
    | RunProgram
    | ExpressionComment UUID String
    | ShowComments UUID
    | HideComments UUID
    | AddComment UUID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RunProgram ->
            ( { model
                | resultOfRun =
                    model.declarations
                        |> List.Extra.find (\( id, _ ) -> id == model.mainId)
                        |> Result.fromMaybe "Couldn't find `main`"
                        |> Result.andThen (\( _, { body } ) -> Result.fromMaybe "Couldn't find main's body" (AnyDict.get uuidDict body model.expressions))
                        |> Result.andThen (editorExpressionToExpression model.expressions)
                        |> Result.andThen evaluate
              }
            , Cmd.none
            )

        AddNewDeclaration index initialName ->
            let
                ( newDeclarationId, newUuidSeeds ) =
                    UUID.step model.uuidSeeds

                ( newBodyId, finalUuidSeeds ) =
                    UUID.step newUuidSeeds
            in
            ( { model
                | uuidSeeds = finalUuidSeeds
                , declarations =
                    List.take index model.declarations
                        ++ ( newDeclarationId
                           , { name = Editor.Field.fromString parseDeclarationName initialName
                             , body = newBodyId
                             }
                           )
                        :: List.drop index model.declarations
                , expressions =
                    AnyDict.insert uuidDict
                        newBodyId
                        (newBlock (EditorExprUnknown ""))
                        model.expressions
              }
            , focusElement (UUID.toString newDeclarationId)
            )

        SetDeclarationName updateId newName ->
            ( { model
                | declarations =
                    updateDeclarationById
                        (\declaration ->
                            { declaration
                                | name = Editor.Field.fromString parseDeclarationName newName
                            }
                        )
                        updateId
                        model.declarations
              }
            , Cmd.none
            )

        SetExpressionRaw exprId newRaw ->
            ( { model
                | expressions =
                    AnyDict.update uuidDict
                        exprId
                        (\expr -> { expr | expression = EditorExprUnknown newRaw })
                        model.expressions
              }
            , Cmd.none
            )

        CalculateExpression exprId ->
            case AnyDict.get uuidDict exprId model.expressions of
                Nothing ->
                    ( model, Cmd.none )

                Just edExpr ->
                    let
                        raw =
                            editorExprRaw edExpr

                        deets :
                            { updatedExpr : EditorBlock
                            , newExprs : AnyDict String UUID EditorBlock
                            , finalUuidSeeds : UUID.Seeds
                            , cmd : Cmd Msg
                            , idToRepoint : Maybe ( UUID, UUID )
                            }
                        deets =
                            case ( parseExpression parseExprUnknown raw, edExpr.expression ) of
                                ( Err _, _ ) ->
                                    { updatedExpr = edExpr
                                    , newExprs = AnyDict.empty
                                    , finalUuidSeeds = model.uuidSeeds
                                    , cmd = Cmd.none
                                    , idToRepoint = Nothing
                                    }

                                ( Ok (ParseExprInt i), EditorExprUnknown _ ) ->
                                    let
                                        ( newRightId, afterRightSeeds ) =
                                            UUID.step model.uuidSeeds
                                    in
                                    { updatedExpr = { edExpr | expression = EditorExprInt raw i newRightId }
                                    , newExprs = AnyDict.singleton uuidDict newRightId (newBlock (EditorExprUnknown ""))
                                    , finalUuidSeeds = afterRightSeeds
                                    , cmd = focusElement (UUID.toString newRightId)
                                    , idToRepoint = Nothing
                                    }

                                ( Ok (ParseExprInt i), EditorExprInt _ _ oldRightId ) ->
                                    { updatedExpr = { edExpr | expression = EditorExprInt raw i oldRightId }
                                    , newExprs = AnyDict.empty
                                    , finalUuidSeeds = model.uuidSeeds
                                    , cmd = Cmd.none
                                    , idToRepoint = Nothing
                                    }

                                ( Ok (ParseExprInt i), EditorExprBinaryOp _ _ oldLeftId oldRightId ) ->
                                    -- { updatedExpr = EditorExprInt raw i oldRightId
                                    -- , newExprs = AnyDict.empty
                                    -- , finalUuidSeeds = model.uuidSeeds
                                    -- , cmd = Cmd.none
                                    -- }
                                    Debug.todo ""

                                ( Ok (ParseExprBinaryOp op), EditorExprUnknown _ ) ->
                                    let
                                        ( newRightId, afterRightSeeds ) =
                                            UUID.step model.uuidSeeds

                                        ( newLeftId, additionalExprChanges, finalUuidSeeds ) =
                                            model.expressions
                                                |> AnyDict.toList uuidDict
                                                |> List.Extra.find
                                                    (\( _, edEx ) ->
                                                        case edEx.expression of
                                                            EditorExprInt _ _ rightUuid ->
                                                                exprId == rightUuid

                                                            _ ->
                                                                False
                                                    )
                                                |> Maybe.andThen
                                                    (\( x, ex ) ->
                                                        case ex.expression of
                                                            EditorExprInt a b _ ->
                                                                let
                                                                    ( i, afterLeftSeeds ) =
                                                                        UUID.step afterRightSeeds
                                                                in
                                                                Just
                                                                    ( x
                                                                    , [ ( x, { ex | expression = EditorExprInt a b i } )
                                                                      , ( i, newBlock (EditorExprUnknown "") )
                                                                      ]
                                                                    , afterLeftSeeds
                                                                    )

                                                            _ ->
                                                                Nothing
                                                    )
                                                |> Maybe.withDefault
                                                    (UUID.step afterRightSeeds
                                                        |> (\( a, b ) ->
                                                                ( a
                                                                , [ ( a, newBlock (EditorExprUnknown "") ) ]
                                                                , b
                                                                )
                                                           )
                                                    )
                                    in
                                    { updatedExpr = { edExpr | expression = EditorExprBinaryOp raw op newLeftId newRightId }
                                    , newExprs = AnyDict.fromList uuidDict (( newRightId, newBlock (EditorExprUnknown "") ) :: additionalExprChanges)
                                    , finalUuidSeeds = finalUuidSeeds
                                    , cmd = focusElement (UUID.toString newRightId)
                                    , idToRepoint = Just ( newLeftId, exprId )
                                    }

                                ( Ok (ParseExprBinaryOp op), EditorExprBinaryOp _ _ oldLeftId oldRightId ) ->
                                    { updatedExpr = { edExpr | expression = EditorExprBinaryOp raw op oldLeftId oldRightId }
                                    , newExprs = AnyDict.empty
                                    , finalUuidSeeds = model.uuidSeeds
                                    , cmd = Cmd.none
                                    , idToRepoint = Nothing
                                    }

                                ( Ok (ParseExprBinaryOp op), EditorExprInt _ _ oldRightId ) ->
                                    { updatedExpr = { edExpr | expression = EditorExprBinaryOp raw op (Debug.todo "") oldRightId }
                                    , newExprs = AnyDict.empty
                                    , finalUuidSeeds = model.uuidSeeds
                                    , cmd = Cmd.none
                                    , idToRepoint = Debug.todo ""
                                    }
                    in
                    ( { model
                        | expressions =
                            AnyDict.union
                                deets.newExprs
                                (AnyDict.insert uuidDict exprId deets.updatedExpr model.expressions)
                        , uuidSeeds = deets.finalUuidSeeds
                        , declarations =
                            case deets.idToRepoint of
                                Nothing ->
                                    model.declarations

                                Just ( from, to ) ->
                                    List.map
                                        (Tuple.mapSecond
                                            (\dec ->
                                                if dec.body == from then
                                                    { dec | body = to }

                                                else
                                                    dec
                                            )
                                        )
                                        model.declarations
                      }
                    , deets.cmd
                    )

        ShowComments exprId ->
            ( { model
                | expressions =
                    AnyDict.update uuidDict
                        exprId
                        (\expr -> { expr | showComments = True })
                        model.expressions
              }
            , Cmd.none
            )

        HideComments exprId ->
            ( { model
                | expressions =
                    AnyDict.update uuidDict
                        exprId
                        (\expr -> { expr | showComments = False })
                        model.expressions
              }
            , Cmd.none
            )

        ExpressionComment exprId comment ->
            ( { model
                | expressions =
                    AnyDict.update uuidDict
                        exprId
                        (\expr -> { expr | newComment = comment })
                        model.expressions
              }
            , Cmd.none
            )

        AddComment exprId ->
            ( { model
                | expressions =
                    AnyDict.update uuidDict
                        exprId
                        (\expr -> { expr | newComment = "", comments = expr.newComment :: expr.comments })
                        model.expressions
              }
            , Cmd.none
            )


focusElement : String -> Cmd Msg
focusElement id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus id)


parseDeclarationName : String -> Result String String
parseDeclarationName rawName =
    if String.isEmpty rawName then
        Err "Declaration names can't be empty"

    else
        Parser.run parseDeclarationNameHelper rawName
            |> Result.mapError (\_ -> "Bad name")


parseDeclarationNameHelper : Parser String
parseDeclarationNameHelper =
    Parser.succeed ()
        |. Parser.spaces
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isLower char)
        |. Parser.chompWhile (\char -> Char.isAlphaNum char || char == '_' || char == '-')
        |> Parser.getChompedString


updateDeclarationById : (EditorDeclaration -> EditorDeclaration) -> UUID -> List ( UUID, EditorDeclaration ) -> List ( UUID, EditorDeclaration )
updateDeclarationById fn updateId =
    List.map
        (\( id, declaration ) ->
            ( id
            , if id == updateId then
                fn declaration

              else
                declaration
            )
        )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Fungi Editor"
    , body = [ layout [ padding 16 ] (viewModel model) ]
    }


viewModel : Model -> Element Msg
viewModel model =
    column
        [ spacing 16 ]
        [ Input.button
            [ Border.solid
            , Border.width 3
            , paddingXY 16 8
            ]
            { label = text "Run"
            , onPress = Just RunProgram
            }
        , case model.resultOfRun of
            Err err ->
                el [ Font.color Editor.Theme.error ] (text err)

            Ok val ->
                el [ Font.color (rgb 0.2 1 0.2) ] (text val)
        , viewDeclarations model.expressions model.mainId model.declarations
        ]


viewDeclarations : Expressions -> UUID -> List ( UUID, EditorDeclaration ) -> Element Msg
viewDeclarations expressions mainId declarations =
    List.foldr
        (\dec ( index, res ) ->
            ( index - 1
            , viewDeclaration expressions mainId declarations dec :: viewAddDeclaration index :: res
            )
        )
        ( List.length declarations, [] )
        declarations
        |> Tuple.second
        |> column [ spacing 32 ]


viewDeclaration : Expressions -> UUID -> List ( UUID, EditorDeclaration ) -> ( UUID, EditorDeclaration ) -> Element Msg
viewDeclaration expressions mainId declarations ( id, declaration ) =
    let
        nameError =
            case Editor.Field.getParsed declaration.name of
                Ok _ ->
                    List.Extra.find
                        (\( decId, { name } ) ->
                            decId /= id && Editor.Field.getRaw name == Editor.Field.getRaw declaration.name
                        )
                        declarations
                        |> Maybe.map (\_ -> "A declaration with this name already exists")

                Err err ->
                    Just err
    in
    row
        [ spacing 8 ]
        [ if id == mainId then
            declaration.name
                |> Editor.Field.getRaw
                |> text

          else
            Editor.Theme.text
                [ Element.Extra.id (UUID.toString id)
                , viewFieldError text nameError
                , case nameError of
                    Nothing ->
                        transparent False

                    Just _ ->
                        Border.color Editor.Theme.error
                ]
                { label = Input.labelHidden "declaration name"
                , text = Editor.Field.getRaw declaration.name
                , onChange = SetDeclarationName id
                , placeholder = Nothing
                }
        , text "is"
        , viewExprElseNone declaration.body expressions
        ]


viewEditorExpression : Expressions -> UUID -> EditorBlock -> Element Msg
viewEditorExpression expressions id editorExpr =
    let
        centerView =
            Editor.Theme.text
                [ Element.Events.onLoseFocus (CalculateExpression id)
                , Element.Extra.id (UUID.toString id)
                , Input.button
                    []
                    { label = text "©"
                    , onPress = Just (ShowComments id)
                    }
                    |> below
                , below <|
                    if editorExpr.showComments then
                        viewComments id editorExpr

                    else
                        none
                ]
                { label = Input.labelHidden "expression"
                , text = editorExprRaw editorExpr
                , onChange = SetExpressionRaw id
                , placeholder = Nothing
                }
    in
    case editorExpr.expression of
        EditorExprUnknown _ ->
            centerView

        EditorExprInt _ _ rightId ->
            row
                [ width fill ]
                [ centerView
                , viewExprElseNone rightId expressions
                ]

        EditorExprBinaryOp _ _ leftId rightId ->
            row
                [ width fill ]
                [ viewExprElseNone leftId expressions
                , centerView
                , viewExprElseNone rightId expressions
                ]


viewComments : UUID -> EditorBlock -> Element Msg
viewComments id editorExpr =
    row
        [ Background.color (rgb 1 1 1)
        , Border.solid
        , Border.width 1
        , Border.rounded 4
        , padding 16
        , spacing 16
        ]
        [ Input.button
            [ alignTop ]
            { label = text "©"
            , onPress = Just (HideComments id)
            }
        , column
            [ alignTop ]
            [ Input.text
                []
                { label = Input.labelAbove [] (text "New Comment")
                , onChange = ExpressionComment id
                , text = editorExpr.newComment
                , placeholder = Nothing
                }
            , Input.button
                []
                { label = text "Add Comment"
                , onPress =
                    if String.isEmpty editorExpr.newComment then
                        Nothing

                    else
                        Just (AddComment id)
                }
            ]
        , editorExpr.comments
            |> List.map text
            |> column [ alignTop ]
        ]


editorExprRaw : EditorBlock -> String
editorExprRaw expr =
    case expr.expression of
        EditorExprUnknown raw ->
            raw

        EditorExprInt raw _ _ ->
            raw

        EditorExprBinaryOp raw _ _ _ ->
            raw


viewExprElseNone : UUID -> Expressions -> Element Msg
viewExprElseNone id expressions =
    case AnyDict.get uuidDict id expressions of
        Nothing ->
            none

        Just expr ->
            viewEditorExpression expressions id expr


viewFieldError : (a -> Element Msg) -> Maybe a -> Attribute Msg
viewFieldError fn maybeErr =
    inFront <|
        el
            [ above <|
                case maybeErr of
                    Nothing ->
                        none

                    Just err ->
                        el
                            [ Font.color Editor.Theme.error ]
                            (fn err)
            , transparent True
            , mouseOver [ transparent False ]
            , width fill
            , height fill
            ]
            none


viewAddDeclaration : Int -> Element Msg
viewAddDeclaration index =
    Editor.Theme.text
        []
        { label = Input.labelHidden "empty declaration"
        , placeholder = Nothing
        , text = ""
        , onChange = AddNewDeclaration index
        }



---- EXPRESSIONS ----


type alias EditorBlock =
    { comments : List String
    , newComment : String
    , showComments : Bool
    , expression : EditorExpression
    }


newBlock : EditorExpression -> EditorBlock
newBlock expr =
    { comments = []
    , newComment = ""
    , showComments = False
    , expression = expr
    }


type EditorExpression
    = EditorExprUnknown String
    | EditorExprInt String Int UUID
    | EditorExprBinaryOp String Op UUID UUID


type ParseExpression
    = ParseExprInt Int
    | ParseExprBinaryOp Op


type Expression
    = ExprInt Int
    | ExprBinaryOp Op Expression Expression


type Op
    = Add
    | Subtract


parseExpression : Parser ParseExpression -> String -> Result String ParseExpression
parseExpression parser =
    Parser.run parser
        >> Result.mapError
            (\err ->
                let
                    _ =
                        Debug.log "parse err" err
                in
                "parse err"
            )


parseExprUnknown : Parser ParseExpression
parseExprUnknown =
    Parser.oneOf
        [ parseExprInt
        , parseExprBinaryOp
        ]


parseExprBinaryOp : Parser ParseExpression
parseExprBinaryOp =
    Parser.succeed ParseExprBinaryOp
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.symbol "+" |> Parser.map (\() -> Add)
            , Parser.symbol "-" |> Parser.map (\() -> Subtract)
            ]
        |. Parser.spaces
        |. Parser.end


parseExprInt : Parser ParseExpression
parseExprInt =
    Parser.succeed ParseExprInt
        |. Parser.spaces
        |= (Parser.succeed ()
                |. Parser.chompIf Char.isDigit
                |. Parser.chompWhile Char.isDigit
                |> Parser.getChompedString
                |> Parser.andThen
                    (\str ->
                        case String.toInt str of
                            Just i ->
                                Parser.succeed i

                            Nothing ->
                                Parser.problem ("Expteced an integer but found: " ++ str)
                    )
           )
        |. Parser.spaces
        |. Parser.end


editorExpressionToExpression : Expressions -> EditorBlock -> Result String Expression
editorExpressionToExpression edExpressions { expression } =
    case expression of
        EditorExprUnknown str ->
            Err ("Unknown expression: " ++ str)

        EditorExprInt _ i _ ->
            Ok (ExprInt i)

        EditorExprBinaryOp _ op leftId rightId ->
            Ok
                (ExprBinaryOp op)
                |> Result.Extra.andMap
                    (Result.fromMaybe "Couldn't find left expression" (AnyDict.get uuidDict leftId edExpressions)
                        |> Result.andThen (editorExpressionToExpression edExpressions)
                    )
                |> Result.Extra.andMap
                    (Result.fromMaybe "Couldn't find right expression" (AnyDict.get uuidDict rightId edExpressions)
                        |> Result.andThen (editorExpressionToExpression edExpressions)
                    )


evaluate : Expression -> Result String String
evaluate expr =
    case expr of
        ExprInt i ->
            Ok (String.fromInt i)

        ExprBinaryOp Add (ExprInt left) (ExprInt right) ->
            evaluate (ExprInt (left + right))

        ExprBinaryOp Subtract (ExprInt left) (ExprInt right) ->
            evaluate (ExprInt (left - right))

        _ ->
            Err "err"
