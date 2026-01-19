module Config
  ( loadProjects
  , getConfigPath
  ) where

import Data.Bifunctor (first)
import Data.Yaml (decodeFileEither, ParseException)
import System.Directory (doesFileExist, getHomeDirectory)
import Types (Project)

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
