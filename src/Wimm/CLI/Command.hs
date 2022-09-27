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

import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as YamlP
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSONP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
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

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand (CTxnReport journalPath reportPath) = 
  runReport journalPath reportPath (transactionReport (Nothing, Nothing))

runCommand (CBalSheetReport journalPath reportPath) =
  runReport journalPath reportPath (balanceSheetReport (Nothing, Nothing))

runCommand (CBudgetReport journalPath reportPath) =
  runReport journalPath reportPath 
  (\j -> 
    let budgets = filter (\b -> bName b == (jDefaultBudget j)) 
                $ jBudgets j
    in if null budgets
       then []
       else budgetReport (Nothing, Nothing) j (head budgets))

runCommand (CIncomeStatementReport journalPath reportPath) =
  runReport journalPath reportPath (incomeStatementReport (Nothing, Nothing))

runCommand (CTxnImport csvDescPath csvDataPath outputPath journalPath) = do
  input <- Yaml.decodeFileEither csvDescPath :: IO (Either Yaml.ParseException ImportCsv)
  case input of
    Left err -> putStrLn (show err)
    Right desc -> do
      txns <- importCsv desc csvDataPath
      case journalPath of
        Nothing -> encodeFileByExt outputPath configTxnJSON configTxnYaml (txns :: [Transaction])
        Just jPath -> do
          journal <- decodeJournal jPath
          case journal of
            Left err1 -> putStrLn (show err1)
            Right j -> encodeFileByExt outputPath configTxnJSON configTxnYaml $ removeDuplicateTxns (jTransactions j) txns

runReport :: FilePath -> FilePath -> (Journal -> Report) -> IO ()
runReport journalPath reportPath mkReport = do
  input <- decodeJournal journalPath
  case input of
    Left err -> putStrLn (show err)
    Right journal -> do
      let report = mkReport journal
      let csvSep = jCsvSeparator $ jReportParams journal
      writeReport reportPath csvSep report

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeJournal :: FilePath -> IO (Either String Journal)
decodeJournal path =
  case map toLower (takeExtension path) of
    ".json" -> JSON.eitherDecodeFileStrict path :: IO (Either String Journal)
    _ -> fmap (either (Left . show) Right) 
         (Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Journal))

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
encodeFileByExt :: (JSON.ToJSON a) => FilePath -> JSONP.Config -> YamlP.Config -> a -> IO ()
encodeFileByExt path jsonConfig yamlConfig x =
  case map toLower (takeExtension path) of
    ".json" -> BL.writeFile path $ JSONP.encodePretty' jsonConfig x
    _ -> BS.writeFile path $ YamlP.encodePretty yamlConfig x

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