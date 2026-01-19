{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types
  (Project(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Project = Project
    { projectName :: String
    , projectPath :: FilePath
    , projectCommand :: String
    , projectPort :: Maybe Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)