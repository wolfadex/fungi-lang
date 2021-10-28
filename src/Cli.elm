module Cli exposing (program)

import Ansi
import Console
import Dict exposing (Dict)
import Fungi exposing (Import, Module, PartialModule)
import Path exposing (Path)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Process


program : Process -> IO ()
program process =
    case ( process.argv, Dict.get "PWD" process.env ) of
        ( _ :: entryFile :: _, Just currentWorkingDirectory ) ->
            let
                entryPath : Path
                entryPath =
                    Path.join currentWorkingDirectory entryFile
            in
            printLn (Console.green "Getting entry file...")
                |> IO.andThen (\() -> readModule entryPath)
                |> exitWithError
                |> IO.andThen
                    (printStatus (Console.green "Parsing entry file...")
                        (\rootPartialModule ->
                            gatherModules rootPartialModule.imports (Dict.singleton entryPath rootPartialModule)
                        )
                    )
                |> exitWithError
                |> IO.map parseModules
                |> exitWithError
                |> IO.andThen (Dict.toList >> List.map (Tuple.second >> Fungi.moduleToString) >> String.join "\n\n" >> printLn)

        _ ->
            printLn "Expected an entry file path"


printStatus : String -> (a -> IO b) -> (a -> IO b)
printStatus msg fn =
    \a -> IO.andThen (\() -> fn a) (printLn msg)


exitWithError : IO (Result String a) -> IO a
exitWithError =
    IO.exitOnError Console.red


parseModules : Dict String PartialModule -> Result String (Dict String Module)
parseModules partialModules =
    praseModulesHelper (Dict.toList partialModules) Dict.empty


praseModulesHelper : List ( String, PartialModule ) -> Dict String Module -> Result String (Dict String Module)
praseModulesHelper partialModules modules =
    case partialModules of
        [] ->
            Ok modules

        ( path, next ) :: rest ->
            case Fungi.parseModule next of
                Err err ->
                    let
                        _ =
                            Debug.log "module body parse error" err
                    in
                    Err "module parse err"

                Ok module_ ->
                    praseModulesHelper rest (Dict.insert path module_ modules)



-- |> IO.andThen
--     (\loadedModulesResult ->
--         case loadedModulesResult of
--             Err err ->
--                 IO.return (Err err)
--             Ok loadedModules ->
--                 Dict.foldl
--                     (\_ partialModule acc ->
--                         Fungi.partialModuleToString partialModule ++ "\n\n" ++ acc
--                     )
--                     ""
--                     loadedModules
--                     |> Ok
--                     |> IO.return
--     )


gatherModules : List Import -> Dict String PartialModule -> IO (Result String (Dict String PartialModule))
gatherModules modulesToLoad loadedModules =
    case modulesToLoad of
        [] ->
            IO.return (Ok loadedModules)

        nextImport :: restOfImports ->
            if Dict.member nextImport.path loadedModules then
                gatherModules restOfImports loadedModules

            else
                readModule nextImport.path
                    |> IO.andThen
                        (\result ->
                            case result of
                                Err err ->
                                    IO.return (Err err)

                                Ok partialModule ->
                                    gatherModules
                                        (restOfImports ++ partialModule.imports)
                                        (Dict.insert nextImport.path partialModule loadedModules)
                        )


readModule : String -> IO (Result String PartialModule)
readModule filePath =
    File.contentsOf filePath
        |> IO.andThen
            (\result ->
                (case result of
                    Err err ->
                        Err err

                    Ok contents ->
                        case Fungi.parseFile filePath contents of
                            Err err ->
                                let
                                    _ =
                                        Debug.log "partial module parse error" err
                                in
                                Err "parse err"

                            Ok partialModule ->
                                Ok partialModule
                )
                    |> IO.return
            )


printLn : String -> IO ()
printLn text =
    Process.print (text ++ "\n")
