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

import Data.Yaml (decodeFileEither, ParseException, encodeFile)
import Data.Aeson (eitherDecodeFileStrict)
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
  input <- decodeFileEither csvDescPath :: IO (Either ParseException ImportCsv)
  case input of
    Left err -> putStrLn (show err)
    Right desc -> do
      txns <- importCsv desc csvDataPath
      case journalPath of
        Nothing -> encodeFile outputPath (txns :: [Transaction])
        Just jPath -> do
          journal <- decodeJournal jPath
          case journal of
            Left err1 -> putStrLn (show err1)
            Right j -> encodeFile outputPath $ removeDuplicateTxns (jTransactions j) txns

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
    ".json" -> eitherDecodeFileStrict path :: IO (Either String Journal)
    _ -> fmap (either (Left . show) Right) (decodeFileEither path :: IO (Either ParseException Journal))