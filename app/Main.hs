module Main (main) where

import System.Environment (getArgs)
import Types (Project (..))

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
listProjects = mapM_ printProject sampleProjects

printProject :: Project -> IO ()
printProject p = putStrLn $ projectName p ++ " - " ++ projectCommand p

sampleProjects :: [Project]
sampleProjects =
    [ Project "web-app" "/home/user/web-app" "npm run dev" (Just 3000)
    , Project "api-server" "/home/user/api" "cargo run" (Just 8080)
    ]

printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "dev-launcher - Development Environment Launcher"
    , ""
    , "Commands:"
    , "  list        List registered projects"
    , "  add <name>  Add a new project"
    , "  help        Show this help"
    ]