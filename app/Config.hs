module Config
  ( loadProjects
  , saveProjects
  , addProject
  , getConfigPath
  ) where

import Data.Bifunctor (first)
import Data.Yaml (decodeFileEither, encodeFile, ParseException)
import System.Directory (doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Types (Project(..))

-- | 設定ファイルのパスを取得（~/.config/dev-launcher/projects.yaml）
getConfigPath :: IO FilePath
getConfigPath = do
    home <- getHomeDirectory
    return $ home ++ "/.config/dev-launcher/projects.yaml"

-- | 設定ファイルからプロジェクト一覧を読み込む
loadProjects :: IO (Either String [Project])
loadProjects = do
    configPath <- getConfigPath
    exists <- doesFileExist configPath
    if not exists
        then return $ Left $ "Config file not found: " ++ configPath
        else first formatError <$> decodeFileEither configPath
  where
    formatError :: ParseException -> String
    formatError err = "YAML parse error: " ++ show err

-- | プロジェクト一覧を設定ファイルに保存する
saveProjects :: [Project] -> IO ()
saveProjects projects = do
    configPath <- getConfigPath
    createDirectoryIfMissing True (takeDirectory configPath)
    encodeFile configPath projects

-- | 新しいプロジェクトを追加する
addProject :: Project -> IO (Either String ())
addProject newProject = do
    result <- loadProjects
    case result of
        Left _ -> do
            -- ファイルがない場合は新規作成
            saveProjects [newProject]
            return $ Right ()
        Right projects ->
            if any (\p -> projectName p == projectName newProject) projects
                then return $ Left $ "Project already exists: " ++ projectName newProject
                else do
                    saveProjects (projects ++ [newProject])
                    return $ Right ()
