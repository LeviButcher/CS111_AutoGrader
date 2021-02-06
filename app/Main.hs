module Main where

import Control.Monad (zipWithM_)
import qualified Data.ByteString.Lazy as BS
import Data.Char (isDigit)
import Data.Csv (encode)
import Data.List (intersperse, isSuffixOf)
import Lib (toProjectResult)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.Process
  ( CreateProcess (cwd),
    callCommand,
    readCreateProcess,
    shell,
  )
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- Change to pass in output directory

tempDir = "./temp"

zipNameToUserName :: String -> String
zipNameToUserName = takeWhile (/= '_') . drop 5

filterZips = filter $ isSuffixOf ".zip"

-- s is path to zipped file
-- d is path to output Dir
unZip :: FilePath -> FilePath -> IO ()
unZip s d = callCommand $ "unzip -o " ++ s ++ " -d " ++ d

-- Runs and returns test result of projects
runProjectTests :: FilePath -> IO String
runProjectTests x = do
  javaFiles <- unwords . lines <$> readCreateProcess (shell "find . -name \"*.java\" ! -path '*/__MACOSX/*'") {cwd = Just x} ""
  let javacCommand = "javac -d out -cp ../../junit-platform-console-standalone-1.7.1.jar " ++ javaFiles
  let runCommand = "java -jar ../../junit-platform-console-standalone-1.7.1.jar --disable-banner --disable-ansi-colors --class-path out --scan-class-path"
  readCreateProcess (shell javacCommand) {cwd = Just x} ""
  readCreateProcess (shell runCommand) {cwd = Just x} ""

parseTestResults :: String -> (Int, Int)
parseTestResults s = (parseSuccess s, parseFound s)
  where
    parseSuccess = read . takeWhile isDigit . (=~ "[0-9]+ tests successful") :: String -> Int
    parseFound = read . takeWhile isDigit . (=~ "[0-9]+ tests found") :: String -> Int

-- need to change to path types

main :: IO ()
main = do
  args <- getArgs
  let dir = head args
  print dir
  zips <- filterZips <$> getDirectoryContents dir
  let names = zipNameToUserName <$> zips
  let zipPath = ((dir ++ "/") ++) <$> zips
  let projectPaths = ((tempDir ++ "/") ++) <$> names
  zipWithM_ unZip zipPath projectPaths
  rawTestResults <- sequence $ runProjectTests <$> projectPaths
  let testResults = parseTestResults <$> rawTestResults
  let projectResults = zipWith3 toProjectResult names testResults rawTestResults
  BS.writeFile "result.csv" $ encode projectResults
