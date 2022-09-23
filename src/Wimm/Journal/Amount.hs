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
      showAmount
    ) where

import Data.Decimal
import Data.Scientific
import qualified Data.Text as T
import Wimm.Journal.ReportParameters

-- | A quantity is any decimal number. The decimal package ensures
-- that no rounding error can occur.
type Amount = Decimal

fromScientific :: Scientific -> Amount
fromScientific = fromRational . toRational

toScientific :: Amount -> Scientific
toScientific = fromRational . toRational

showAmount :: JournalReportParameters -> Amount -> T.Text
showAmount p = T.pack 
                . map (\x -> if x == '.' then jDecimalSep p else x) 
                . show