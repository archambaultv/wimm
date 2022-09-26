-- |
-- Module      :  Wimm.CLI.CLI
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the command line interface of wimm

module Wimm.CLI.CLI
(
  cli
)

where

import Options.Applicative
import Wimm.CLI.Command

journalFile :: Parser String
journalFile = argument str (metavar "JOURNAL-FILE" <> help "The journal file")

csvDescFile :: Parser String
csvDescFile = argument str (metavar "CSV-DESC-FILE" <> help "The YAML file that describes the csv structures")

statementFile :: Parser String
statementFile = argument str (metavar "STATEMENT-FILE" <> help "The csv file from the bank")

csvFile :: Parser String
csvFile = argument str (metavar "CSV-FILE" <> help "The output csv file")

transactionsReport :: Parser Command
transactionsReport = CTxnReport
                   <$> journalFile
                   <*> csvFile

transactionsReportInfo :: ParserInfo Command
transactionsReportInfo = info (transactionsReport <**> helper)
              (fullDesc
               <> progDesc "Prints all transactions in a CSV format")

balanceSheetReport :: Parser Command
balanceSheetReport = CBalSheetReport
                   <$> journalFile
                   <*> csvFile

balanceSheetReportInfo :: ParserInfo Command
balanceSheetReportInfo = info (balanceSheetReport <**> helper)
              (fullDesc
               <> progDesc "Prints the balance sheet in a CSV format")

transactionsImport :: Parser Command
transactionsImport = CTxnImport
                   <$> csvDescFile
                   <*> statementFile
                   <*> csvFile

transactionsImportInfo :: ParserInfo Command
transactionsImportInfo = info (transactionsImport <**> helper)
              (fullDesc
               <> progDesc "Imports transactions from a bank CSV and prints them in a YAML format")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "balancesheet" balanceSheetReportInfo <>
    command "transactions" transactionsReportInfo <>
    command "import" transactionsImportInfo
    )

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use wimm -h to list the \
                     \possible commands. Use wimm COMMAND -h for help \
                     \on a specific command."
         <> header "Wimm - Simple and git friendly accounting command line tool")

cli :: IO ()
cli = execParser opts >>= runCommand