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

import Data.Yaml (decodeFileEither, ParseException)
import Wimm.Journal
import Wimm.Report

-- | The commands accepted by the command line interface
data Command = Command {
  cJournalFile :: String, -- The journal file
  cCsvFile :: FilePath -- The output file filepath
  }

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = do
  input <- decodeFileEither (cJournalFile c) :: IO (Either ParseException Journal)
  case input of
    Left err -> putStrLn (show err)
    Right journal -> writeReport (cCsvFile c) (jCsvSeparator journal)
                   $ transactionReport (Nothing, Nothing) journal