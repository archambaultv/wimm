-- |
-- Module      :  Wimm.Journal.Amount
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what an amount is.

module Wimm.Journal.Amount
    ( Amount,
      fromScientific,
      toScientific,
      showAmount,
      readAmount
    ) where

import Data.Decimal (Decimal)
import Data.Scientific (scientificP, Scientific)
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Text as T
import Wimm.Journal.ReportParameters

-- | A quantity is any decimal number. The decimal package ensures
-- that no rounding error can occur.
type Amount = Decimal

fromScientific :: Scientific -> Amount
fromScientific = fromRational . toRational

toScientific :: Amount -> Scientific
toScientific = fromRational . toRational

readAmount :: String -> Either String Amount
readAmount s = 
  case readP_to_S scientificP s of
    [(n,"")] -> return $ fromScientific n
    _ -> Left $ "Unable to read " ++ s ++ " has an amount."


showAmount :: JournalReportParameters -> Amount -> T.Text
showAmount p = T.pack 
                . map (\x -> if x == '.' then jDecimalSep p else x) 
                . show