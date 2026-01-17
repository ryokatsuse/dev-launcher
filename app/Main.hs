module Main (main) where

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run []             = putStrLn "Usage: dev-launcher <command>"
run ["list"]       = putStrLn "Listing projects... (not implemented yet)"
run ["add", name]  = putStrLn $ "Adding project: " ++ name
run ["help"]       = printHelp
run args           = putStrLn $ "Unknown command: " ++ unwords args

printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "dev-launcher - Development Environment Launcher"
    , ""
    , "Commands:"
    , "  list        List registered projects"
    , "  add <name>  Add a new project"
    , "  help        Show this help"
    ]
