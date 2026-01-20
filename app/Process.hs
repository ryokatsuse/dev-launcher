module Process
  ( startProject
  , startProjectWithPrefix
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import System.IO (Handle, hGetLine, hIsEOF)
import System.Process (createProcess, shell, CreateProcess(..), StdStream(..), ProcessHandle)
import Types (Project(..))

-- | プロジェクトのコマンドを起動する（シンプル版）
startProject :: Project -> IO ()
startProject project = do
    printProjectInfo project
    let processConfig = (shell (projectCommand project))
            { cwd = Just (projectPath project)
            }
    _ <- createProcess processConfig
    return ()

-- | プレフィックス付きでプロジェクトを起動する
startProjectWithPrefix :: Project -> IO ProcessHandle
startProjectWithPrefix project = do
    printProjectInfo project
    let prefix = "[" ++ projectName project ++ "] "
    let processConfig = (shell (projectCommand project))
            { cwd = Just (projectPath project)
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    (_, Just hout, Just herr, ph) <- createProcess processConfig

    -- stdout を読み取るスレッド
    void $ forkIO $ readWithPrefix prefix hout

    -- stderr を読み取るスレッド
    void $ forkIO $ readWithPrefix (prefix ++ "ERR: ") herr

    return ph
  where
    readWithPrefix :: String -> Handle -> IO ()
    readWithPrefix prefix h = do
        eof <- hIsEOF h
        if eof
            then return ()
            else do
                line <- hGetLine h
                putStrLn $ prefix ++ line
                readWithPrefix prefix h

-- | プロジェクト情報を表示
printProjectInfo :: Project -> IO ()
printProjectInfo project = do
    putStrLn $ "Starting " ++ projectName project ++ "..."
    putStrLn $ "  Directory: " ++ projectPath project
    putStrLn $ "  Command: " ++ projectCommand project
    case projectPort project of
        Just port -> putStrLn $ "  Port: " ++ show port
        Nothing -> return ()
    putStrLn ""
