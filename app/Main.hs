module Main (main) where

import System.Environment (getArgs)
import Types (Project (..))
import Config (loadProjects, addProject, findProject, getConfigPath)
import Process (startProject)

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run []             = putStrLn "Usage: dev-launcher <command>"
run ["list"]       = listProjects
run ["add", name, path, cmd] = addProjectCmd name path cmd Nothing
run ["add", name, path, cmd, port] = addProjectCmd name path cmd (Just $ read port)
run ["start", name] = startProjectCmd name
run ["help"]       = printHelp
run args           = putStrLn $ "Unknown command: " ++ unwords args

listProjects :: IO ()
listProjects = do
    result <- loadProjects
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right projects -> mapM_ printProject projects

printProject :: Project -> IO ()
printProject p = do
    putStrLn $ projectName p ++ " (" ++ projectPath p ++ ")"
    putStrLn $ "  Command: " ++ projectCommand p
    putStrLn $ "  Port: " ++ maybe "none" show (projectPort p)
    putStrLn ""

addProjectCmd :: String -> FilePath -> String -> Maybe Int -> IO ()
addProjectCmd name path cmd port = do
    let project = Project
            { projectName = name
            , projectPath = path
            , projectCommand = cmd
            , projectPort = port
            }
    result <- addProject project
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right () -> putStrLn $ "Added project: " ++ name

startProjectCmd :: String -> IO ()
startProjectCmd name = do
    result <- findProject name
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right project -> startProject project

printHelp :: IO ()
printHelp = do
    configPath <- getConfigPath
    putStrLn $ unlines
        [ "dev-launcher - Development Environment Launcher"
        , ""
        , "Commands:"
        , "  list                              List registered projects"
        , "  add <name> <path> <cmd> [port]    Add a new project"
        , "  start <name>                      Start a project"
        , "  help                              Show this help"
        , ""
        , "Config file: " ++ configPath
        ]