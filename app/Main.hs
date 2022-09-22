-- |
-- Module      :  Main
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- The wimm command line tool executable

module Main (main) where

import Wimm.CLI

main :: IO ()
main = cli
