-- |
-- Module      :  Wimm.CLI.Commands
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the commands available when using wimm

module Wimm.CLI.Command
(
  Command(..),
  runCommand
) where

import Control.Monad.Except (runExceptT, ExceptT(..), lift, liftEither)
import qualified Data.Yaml as Yaml
import qualified Data.Csv as Csv
import qualified Data.Yaml.Pretty as YamlP
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSONP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Char (ord)
import Wimm.Journal
import Wimm.Import.Csv
import Wimm.Report
import Data.Char (toLower)
import System.FilePath (takeExtension)

-- | The commands accepted by the command line interface
data Command = CTxnReport FilePath FilePath
             | CBalSheetReport FilePath FilePath
             | CBudgetReport FilePath FilePath
             | CIncomeStatementReport FilePath FilePath
             | CTxnImport FilePath FilePath FilePath (Maybe FilePath)
             | CCheck FilePath


-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either putStrLn return

runCommand' :: Command -> ExceptT String IO ()
runCommand' (CTxnReport journalPath reportPath) = 
  runReport journalPath reportPath (transactionReport (Nothing, Nothing))

runCommand' (CBalSheetReport journalPath reportPath) =
  runReport journalPath reportPath (balanceSheetReport (Nothing, Nothing))

runCommand' (CBudgetReport journalPath reportPath) =
  let myReport j = budgetReport (Nothing, Nothing) j (getDefaultBudget j)
  in runReport journalPath reportPath myReport

runCommand' (CIncomeStatementReport journalPath reportPath) =
  runReport journalPath reportPath (incomeStatementReport (Nothing, Nothing))

runCommand' (CTxnImport csvDescPath csvDataPath outputPath journalPath) = do
  -- Decode the csv description and the csv file itself
  desc <- decodeFileByExt csvDescPath
  csv <- lift $ BL.readFile csvDataPath
  let (h,lineOffset) = if iSkipHeader desc then (Csv.HasHeader, 1) else (Csv.NoHeader, 0)
  let opt = Csv.defaultDecodeOptions {
      Csv.decDelimiter = fromIntegral (ord (iCsvSeparator desc))
    }
  csvLines <- liftEither (Csv.decodeWith opt h csv :: Either String (V.Vector (V.Vector T.Text)))
  
  -- Verify if we have to filter with existing transactions
  myfilter <- case journalPath of
              Nothing -> pure []
              Just jPath -> do
                j <- decodeJournal jPath
                pure $ jTransactions j

  -- Extract the transactions from the csv file
  lineResults <- liftEither $ importTxns desc csvLines lineOffset myfilter
  txns <- lift $ printImportReport lineResults

  -- Build report and write transactions to file
  encodeFileByExt outputPath configTxnJSON configTxnYaml txns

runCommand' (CCheck journalPath) = do
  _ <- decodeJournal journalPath -- Decode journal calls journalCheck
  lift $ putStrLn "Journal file OK"

printImportReport :: [CsvLineResult] -> IO [Transaction]
printImportReport xs =
  let nbOfLines = show $ length xs
      nbOfRejected = show $ length $ filter csvResultIsRejected xs
      nbOfDuplicates = show $ length $ filter csvResultIsDuplicate xs
      nbOfMatch = show $ length $ filter csvResultIsMatch xs
      nbOfDefault = show $ length $ filter csvResultIsDefault xs
      width = maximum $ map length [nbOfLines, nbOfRejected, nbOfDuplicates, nbOfMatch, nbOfDefault]
      pad :: String -> String
      pad s = replicate (width - length s) ' ' ++ s
  in do
    putStrLn $ "Number of csv lines       : " ++ pad nbOfLines
    putStrLn $ "Number of rejected lines  : " ++ pad nbOfRejected
    putStrLn $ "Number of duplicate lines : " ++ pad nbOfDuplicates
    putStrLn $ "Number of matched lines   : " ++ pad nbOfMatch
    putStrLn $ "Number of default lines   : " ++ pad nbOfDefault
    return $ csvAcceptedResult xs




runReport :: FilePath -> FilePath -> (Journal -> Report) -> ExceptT String IO ()
runReport journalPath reportPath mkReport = do
  journal <- decodeJournal journalPath
  let report = mkReport journal
  let csvSep = jCsvSeparator $ jReportParams journal
  lift $ writeReport reportPath csvSep report

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeJournal :: FilePath -> ExceptT String IO Journal
decodeJournal path = do
  j <- ExceptT $ case map toLower (takeExtension path) of
                ".json" -> JSON.eitherDecodeFileStrict path :: IO (Either String Journal)
                _ -> fmap (either (Left . show) return) 
                    (Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Journal))
  liftEither $ journalCheck j
  return j

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
encodeFileByExt :: (JSON.ToJSON a) => FilePath -> JSONP.Config -> YamlP.Config -> a -> ExceptT String IO ()
encodeFileByExt path jsonConfig yamlConfig x = lift $
  case map toLower (takeExtension path) of
    ".json" -> BL.writeFile path $ JSONP.encodePretty' jsonConfig x
    _ -> BS.writeFile path $ YamlP.encodePretty yamlConfig x

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeFileByExt :: (JSON.FromJSON a) => FilePath -> ExceptT String IO a
decodeFileByExt path  = ExceptT $ 
  case map toLower (takeExtension path) of
    ".json" -> JSON.eitherDecodeFileStrict' path
    _ -> fmap (either (Left . show) return) $ Yaml.decodeFileEither path

txnConfCompare :: T.Text -> T.Text -> Ordering
txnConfCompare = JSONP.keyOrder ["date","postings","comment","counterparty","tags","statement description"]
                      `mappend` compare 

configTxnYaml :: YamlP.Config
configTxnYaml = YamlP.setConfCompare txnConfCompare YamlP.defConfig

configTxnJSON :: JSONP.Config
configTxnJSON = JSONP.defConfig { 
  JSONP.confIndent = JSONP.Spaces 2, 
  JSONP.confCompare = txnConfCompare
  }