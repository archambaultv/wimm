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
csvDescFile = argument str (metavar "STATEMENT-DESC-FILE" <> help "The JSON/YAML file that describes the csv STATEMENT-FILE")

statementFile :: Parser String
statementFile = argument str (metavar "STATEMENT-FILE" <> help "The csv file from the bank")

csvOutputFile :: Parser String
csvOutputFile = argument str (metavar "CSV-OUTPUT-FILE" <> help "The output csv file")

outputFile :: Parser String
outputFile = argument str (metavar "OUTPUT-FILE" <> help "The output file")

transactionsReport :: Parser Command
transactionsReport = CTxnReport
                   <$> journalFile
                   <*> csvOutputFile

transactionsReportInfo :: ParserInfo Command
transactionsReportInfo = info (transactionsReport <**> helper)
              (fullDesc
               <> progDesc "Prints all transactions in a CSV format")

balanceSheetReport :: Parser Command
balanceSheetReport = CBalSheetReport
                   <$> journalFile
                   <*> csvOutputFile

balanceSheetReportInfo :: ParserInfo Command
balanceSheetReportInfo = info (balanceSheetReport <**> helper)
              (fullDesc
               <> progDesc "Prints the balance sheet in a CSV format")

incomeStatementReport :: Parser Command
incomeStatementReport = CIncomeStatementReport
                   <$> journalFile
                   <*> csvOutputFile

incomeStatementReportInfo :: ParserInfo Command
incomeStatementReportInfo = info (incomeStatementReport <**> helper)
              (fullDesc
               <> progDesc "Prints the income statement sheet in a CSV format")

transactionsImport :: Parser Command
transactionsImport = CTxnImport
                   <$> csvDescFile
                   <*> statementFile
                   <*> outputFile
                   <*> optional journalFile

transactionsImportInfo :: ParserInfo Command
transactionsImportInfo = info (transactionsImport <**> helper)
              (fullDesc
               <> progDesc "Imports transactions from a bank CSV and prints them in a JSON/YAML format. If JOURNAL-FILE is provided then wimm will not output existing transactions.")

budgetReport :: Parser Command
budgetReport = CBudgetReport
                   <$> journalFile
                   <*> csvOutputFile

budgetReportInfo :: ParserInfo Command
budgetReportInfo = info (budgetReport <**> helper)
              (fullDesc
               <> progDesc "Prints the budget report in a CSV format")

checkJournal :: Parser Command
checkJournal = CCheck
                   <$> journalFile

checkJournalInfo :: ParserInfo Command
checkJournalInfo = info (checkJournal <**> helper)
              (fullDesc
               <> progDesc "Verify the integrety of the journal file")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "balancesheet" balanceSheetReportInfo <>
    command "budget" budgetReportInfo <>
    command "check" checkJournalInfo <>
    command "import" transactionsImportInfo <>
    command "incomestatement" incomeStatementReportInfo <>
    command "transactions" transactionsReportInfo
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