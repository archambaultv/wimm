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

csvFile :: Parser String
csvFile = argument str (metavar "CSV-FILE" <> help "The csv file")

transactionsCommand :: Parser Command
transactionsCommand = Command
                   <$> journalFile
                   <*> csvFile

transactionsInfo :: ParserInfo Command
transactionsInfo = info (transactionsCommand <**> helper)
              (fullDesc
               <> progDesc "Prints all transactions in a CSV format")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "transactions" transactionsInfo )

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use wimm -h to list the \
                     \possible commands. Use wimm COMMAND -h for help \
                     \on a specific command."
         <> header "Wimm - Simple and git friendly accounting command line tool")

cli :: IO ()
cli = execParser opts >>= runCommand