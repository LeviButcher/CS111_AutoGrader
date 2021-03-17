module Main where

import Control.Applicative (liftA)
import Control.Monad (zipWithM, zipWithM_)
import qualified Data.ByteString.Lazy as BS
import Data.Char (isDigit)
import Data.Csv (encode, encodeDefaultOrderedByName)
import Data.List (intersperse, isSuffixOf)
import GHC.IO.Exception
import Lib (toProjectResult)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.Process
  ( CreateProcess (cwd),
    callCommand,
    readCreateProcess,
    readCreateProcessWithExitCode,
    shell,
  )
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

zipNameToUserName :: String -> String
zipNameToUserName = takeWhile (/= '_') . drop 5

filterZips = filter $ isSuffixOf ".zip"

-- s is path to zipped file
-- d is path to output Dir
unZip :: FilePath -> FilePath -> IO (Either String String)
unZip s d = do
  let unzipCommand = "unzip -o " ++ "\"" ++ s ++ "\"" ++ " -d " ++ d
  (c, _, stdErr) <- readCreateProcessWithExitCode (shell unzipCommand) ""
  return $ case c of
    ExitSuccess -> Right d
    _ -> Left stdErr

-- Runs and returns test result of projects
runProjectTests :: FilePath -> IO (Either String String)
runProjectTests x = do
  (code, stdOut, stdErr) <- readCreateProcessWithExitCode (shell "find . -name \"*.java\" ! -path '*/__MACOSX/*'") {cwd = Just x} ""
  let javaFiles = unwords . lines $ stdOut
  let javacCommand = "javac -d out -cp ../../junit-platform-console-standalone-1.7.1.jar " ++ javaFiles
  let runCommand = "java -jar ../../junit-platform-console-standalone-1.7.1.jar --disable-banner --disable-ansi-colors --class-path out --scan-class-path"
  if not $ null stdErr
    then return (Left stdErr)
    else do
      print $ "Compiling project for: " ++ x
      (code, stdOut, stdErr) <- readCreateProcessWithExitCode (shell javacCommand) {cwd = Just x} ""
      if not $ null stdErr
        then return (Left stdErr)
        else do
          print $ "Running tests for:" ++ x
          (code, stdOut, stdErr) <- readCreateProcessWithExitCode (shell runCommand) {cwd = Just x} ""
          return (Right stdOut)

parseTestResults :: String -> (Int, Int)
parseTestResults s = (parseSuccess s, parseFound s)
  where
    parseSuccess = read . takeWhile isDigit . (=~ "[0-9]+ tests successful") :: String -> Int
    parseFound = read . takeWhile isDigit . (=~ "[0-9]+ tests found") :: String -> Int

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "Not enough args, stranger"
    (dir : outputDir : _) -> do
      zips <- filterZips <$> getDirectoryContents dir
      let names = zipNameToUserName <$> zips
      let zipPath = ((dir ++ "/") ++) <$> zips
      let projectPaths = ((outputDir ++ "/") ++) <$> names
      unZippedProjectPaths <- zipWithM unZip zipPath projectPaths
      rawTestResults <- sequence $ runProjectTests <$> projectPaths
      let testResults = liftA parseTestResults <$> rawTestResults
      let projectResults = zipWith3 toProjectResult names testResults rawTestResults
      BS.writeFile (outputDir ++ "/result.csv") $ encodeDefaultOrderedByName projectResults
