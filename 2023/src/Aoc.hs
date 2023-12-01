module Aoc (runDay) where

import Common
import Network.HTTP.Simple (httpBS, parseRequest, addRequestHeader, getResponseBody)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.CaseInsensitive as CI
import System.Directory (doesFileExist)
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)

downloadAOCInput :: Int -> IO ()
downloadAOCInput dayNum = do
  loadFile defaultConfig
  putStrLn $ "Downloading day " ++ show dayNum ++ "'s input..."
  initReq <- (parseRequest $ "GET https://adventofcode.com/2023/day/" ++ show dayNum ++ "/input")
  session <- getEnv "SESSION"
  let headedRequest = addRequestHeader (CI.mk $ BSU.fromString "cookie") (BSU.fromString $ "session=" <> session) initReq
  
  response <- getResponseBody <$> httpBS headedRequest
  BS.writeFile ("input/day" ++ show dayNum ++ ".txt") response
  putStrLn "Input Downloaded!"
  return ()

downloadAOCInputMaybe :: Int -> IO ()
downloadAOCInputMaybe dayNum = do
  fileExists <- doesFileExist ("input/day" ++ show dayNum ++ ".txt")
  if fileExists
    then return ()
    else downloadAOCInput dayNum

days :: [Day]
days = []

runDay :: Int -> IO (Int, Int)
runDay dayNum = do
  downloadAOCInputMaybe dayNum
  putStrLn $ "Running day " ++ show dayNum ++ "..."
  input <- readFile $ "input/day" ++ show dayNum ++ ".txt"
  return $ (days !! (dayNum - 1)) input
