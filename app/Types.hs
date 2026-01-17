module Types
  (Project(..)
  ) where

data Project = Project
    { projectName :: String
    , projectPath :: FilePath
    , projectCommand :: String
    , projectPort :: Maybe Int
    }