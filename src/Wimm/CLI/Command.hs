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

import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as B
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
  input <- B.readFile (cJournalFile c)
  let x = eitherDecode' input :: Either String Journal
  case x of
    Left err -> putStrLn err
    Right journal -> writeReport (cCsvFile c) ',' $ transactionReport (Nothing, Nothing) journal