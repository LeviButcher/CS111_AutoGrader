{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( toProjectResult,
  )
where

import Data.Csv (DefaultOrdered, FromNamedRecord, ToNamedRecord, ToRecord)
import Data.Text (Text)
import GHC.Generics (Generic)

data ProjectResult = ProjectResult
  { username :: String,
    testsPassing :: Int,
    totalTests :: Int,
    grade :: Double,
    testResults :: String,
    cliFailures :: String
  }
  deriving (Show, Generic)

instance FromNamedRecord ProjectResult

instance ToRecord ProjectResult

instance ToNamedRecord ProjectResult

instance DefaultOrdered ProjectResult

maxTestPoints = 16

maxStylePoints = 4

toProjectResult :: String -> Either String (Int, Int) -> Either String String -> ProjectResult
toProjectResult s (Right (pass, found)) resultOut =
  ProjectResult
    { username = s,
      testsPassing = pass,
      totalTests = found,
      grade = fromIntegral pass / fromIntegral found * maxTestPoints + maxStylePoints,
      cliFailures = "none",
      testResults = either id id resultOut
    }
toProjectResult s (Left e) resultOut =
  ProjectResult
    { username = s,
      cliFailures = e,
      testsPassing = 0,
      totalTests = 0,
      grade = 0,
      testResults = either id id resultOut
    }