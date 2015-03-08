{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative ((<$>))
import           Control.Monad (forM, forM_)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Error
import           Data.List(foldl')
import qualified Data.Text.Lazy as TL
import           System.Directory (doesFileExist)
import           System.Environment(getArgs)
import           BugDB

main :: IO ()
main = do
  args <- getArgs
  tryRunCommand args

printUsageMessage :: IO ()
printUsageMessage = putStrLn $ "Usage:\n" 
                    ++ "  bugs add dbfile inputfiles...\n"
                    ++ "  bugs count dbfile\n"
                    ++ "  bugs risk dbfile frequencyfile countryfile\n"

tryRunCommand :: [String] -> IO ()
tryRunCommand [] = printUsageMessage
tryRunCommand (cmd : args) =
    let found = lookup cmd commands in
    case found of
         Nothing -> printUsageMessage
         Just command -> runCommand $ command args

runCommand :: ErrorT String IO () -> IO ()
runCommand c = do
  result <- runErrorT c
  case result of
    Right _ -> return ()
    Left message -> putStrLn message

commands :: [(String, [String] -> ErrorT String IO ())]
commands = [ ("add", addCommand)
           , ("count", countCommand)
           , ("risk", riskCommand)
           ]

addCommand :: [String] -> ErrorT String IO ()
addCommand (dbPath : inputPaths @ (_:_)) = do
  dbExists <- lift $ doesFileExist dbPath
  db <- if dbExists then readDb dbPath
        else return emptyDb
  (infos :: [BugInfo]) <- forM inputPaths $ \path ->
          (ErrorT $ text2BugInfo <$> readFileUtf8 path)
  let newDb = foldl' (flip addBug) db infos
  lift $ writeFileUtf8 dbPath (db2csv newDb)
addCommand _ = lift printUsageMessage

countCommand :: [String] -> ErrorT String IO ()
countCommand [dbPath] = do
  db <- readDb dbPath
  let records = bugsPerCountry db
  lift $ forM_ records $ \(country, count) ->
      putStrLn $ TL.unpack country ++ ": " ++ show count
countCommand _ = lift printUsageMessage

riskCommand :: [String] -> ErrorT String IO ()
riskCommand [dbPath, freqPath, countryPath] = do
  db <- readDb dbPath
  frequencies <- readCoefficientFile freqPath
  countries <- readCoefficientFile countryPath
  let records = riskCoefficients db countries frequencies
  lift $ forM_ records $ \(bug, coefficient) ->
      putStrLn $ TL.unpack bug ++ ": " ++ show coefficient
riskCommand _ = lift printUsageMessage
