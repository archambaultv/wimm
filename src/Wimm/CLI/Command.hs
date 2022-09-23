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
import Wimm.Journal
import Wimm.Import.Csv
import Wimm.Report

-- | The commands accepted by the command line interface
data Command = CTxnReport FilePath FilePath
             | CTxnImport FilePath FilePath FilePath

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand (CTxnReport journalPath reportPath) = do
  input <- decodeFileEither journalPath :: IO (Either ParseException Journal)
  case input of
    Left err -> putStrLn (show err)
    Right journal -> do
      let report = transactionReport (Nothing, Nothing) journal
      let csvSep = jCsvSeparator $ jReportParams journal
      writeReport reportPath csvSep report

runCommand (CTxnImport csvDescPath csvDataPath outputPath) = do
  input <- decodeFileEither csvDescPath :: IO (Either ParseException ImportCsv)
  case input of
    Left err -> putStrLn (show err)
    Right desc -> do
      txns <- importCsv desc csvDataPath
      encodeFile outputPath (txns :: [Transaction])