-- |
-- Module      :  Wimm.Report.Report
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the report data type and helper functions related to reports

module Wimm.Report.Report
(
  Report,
  ReportRow,
  ReportPeriod,
  ReportParams(..),
  writeReport,
  toISO8601,
  -- RAccount(..),
  -- rIdentifier,
  -- rDisplayName,
  -- rNumber,
  -- rIsIncomeStatementType,
  -- rIsBalanceSheetType,
  -- accountMap
  )
where

import Data.Char (ord)
import Data.Time (Day, formatTime, defaultTimeLocale, iso8601DateFormat)
import qualified Data.Csv as C
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

-- | A report is a two dimensionnal array, much like an spreedsheet containing text
type Report = [ReportRow]
type ReportRow = [T.Text]

-- | Determines the start date and end date of the reports
type ReportPeriod = (Maybe Day, Maybe Day)

-- | The various type of reports available and their parameters
data ReportParams 
  = Transactions ReportPeriod
  deriving (Eq, Show)

-- | Encode a report as a CSV file
writeReport :: Char -> Report -> BL.ByteString
writeReport c report =
  let myOptions = C.defaultEncodeOptions {
                      C.encDelimiter = fromIntegral (ord c)
                    }
  in C.encodeWith myOptions report

-- | Formats a date for reporting
toISO8601 :: Day -> String
toISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)