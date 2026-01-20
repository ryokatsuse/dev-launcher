module Main (main) where

import System.Environment (getArgs)
import Types (Project (..))
import Control.Monad (forM)
import Data.Either (partitionEithers)
import System.Process (waitForProcess)
import Config (loadProjects, addProject, removeProject, updateProject, findProject, getConfigPath)
import Process (startProject, startProjectWithPrefix)
import Tui (runTui)

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
run ("start-all" : names) = startMultipleCmd names
run ("tui" : names) = tuiCmd names
run ["remove", name] = removeProjectCmd name
run ["edit", name, path, cmd] = editProjectCmd name path cmd Nothing
run ["edit", name, path, cmd, port] = editProjectCmd name path cmd (Just $ read port)
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

removeProjectCmd :: String -> IO ()
removeProjectCmd name = do
    result <- removeProject name
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right () -> putStrLn $ "Removed project: " ++ name

editProjectCmd :: String -> FilePath -> String -> Maybe Int -> IO ()
editProjectCmd name path cmd port = do
    let newProject = Project
            { projectName = name
            , projectPath = path
            , projectCommand = cmd
            , projectPort = port
            }
    result <- updateProject name newProject
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right () -> putStrLn $ "Updated project: " ++ name

startMultipleCmd :: [String] -> IO ()
startMultipleCmd [] = putStrLn "Usage: dev-launcher start-all <name1> <name2> ..."
startMultipleCmd names = do
    -- 各プロジェクトを検索
    results <- forM names findProject
    let (errors, projects) = partitionEithers results

    -- エラーがあれば表示
    mapM_ (\err -> putStrLn $ "Error: " ++ err) errors

    -- プロジェクトを起動してハンドルを取得
    handles <- forM projects startProjectWithPrefix

    -- すべてのプロセスが終了するまで待機
    putStrLn $ "Started " ++ show (length projects) ++ " project(s). Press Ctrl+C to stop."
    mapM_ waitForProcess handles

tuiCmd :: [String] -> IO ()
tuiCmd [] = putStrLn "Usage: dev-launcher tui <name1> <name2> ..."
tuiCmd names = do
    results <- forM names findProject
    let (errors, projects) = partitionEithers results
    mapM_ (\err -> putStrLn $ "Error: " ++ err) errors
    if null projects
        then putStrLn "No valid projects to start"
        else runTui projects

printHelp :: IO ()
printHelp = do
    configPath <- getConfigPath
    putStrLn $ unlines
        [ "dev-launcher - Development Environment Launcher"
        , ""
        , "Commands:"
        , "  list                              List registered projects"
        , "  add <name> <path> <cmd> [port]    Add a new project"
        , "  remove <name>                     Remove a project"
        , "  edit <name> <path> <cmd> [port]   Edit a project"
        , "  start <name>                      Start a project"
        , "  start-all <name1> <name2> ...     Start multiple projects"
        , "  tui <name1> <name2> ...           Start with TUI (split panes)"
        , "  help                              Show this help"
        , ""
        , "Config file: " ++ configPath
        ]