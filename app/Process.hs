module Process
  ( startProject
  ) where

import System.Process (createProcess, shell, CreateProcess(..))
import Types (Project(..))

-- | プロジェクトのコマンドを起動する
startProject :: Project -> IO ()
startProject project = do
    putStrLn $ "Starting " ++ projectName project ++ "..."
    putStrLn $ "  Directory: " ++ projectPath project
    putStrLn $ "  Command: " ++ projectCommand project
    case projectPort project of
        Just port -> putStrLn $ "  Port: " ++ show port
        Nothing -> return ()
    putStrLn ""

    let processConfig = (shell (projectCommand project))
            { cwd = Just (projectPath project)
            }
    _ <- createProcess processConfig
    return ()
