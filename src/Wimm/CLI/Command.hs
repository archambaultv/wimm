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

-- | The commands accepted by the command line interface
data Command = Command {
  cJournalFile :: String -- The journal file
  }

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand _ = putStrLn "Hellow World"