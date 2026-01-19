module Main (main) where

import System.Environment (getArgs)
import Types (Project (..))
import Config (loadProjects, getConfigPath)

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run []             = putStrLn "Usage: dev-launcher <command>"
run ["list"]       = listProjects
run ["add", name]  = putStrLn $ "Adding project: " ++ name
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

printHelp :: IO ()
printHelp = do
    configPath <- getConfigPath
    putStrLn $ unlines
        [ "dev-launcher - Development Environment Launcher"
        , ""
        , "Commands:"
        , "  list        List registered projects"
        , "  add <name>  Add a new project"
        , "  help        Show this help"
        , ""
        , "Config file: " ++ configPath
        ]