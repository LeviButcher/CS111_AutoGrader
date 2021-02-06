{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( toProjectResult,
  )
where

import Data.Csv (ToRecord)
import Data.Text (Text)
import GHC.Generics (Generic)

data ProjectResult = ProjectResult
  { username :: String,
    testsPassing :: Int,
    totalTests :: Int,
    grade :: Double,
    testResults :: String
  }
  deriving (Show, Generic)

instance ToRecord ProjectResult

toProjectResult :: String -> (Int, Int) -> String -> ProjectResult
toProjectResult s (pass, found) resultOut =
  ProjectResult
    { username = s,
      testsPassing = pass,
      totalTests = found,
      grade = fromIntegral pass / fromIntegral found,
      testResults = resultOut
    }