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
  RAccount(..),
  rIdentifier,
  rDisplayName,
  rNumber,
  rIsIncomeStatementType,
  rIsBalanceSheetType,
  accountMap
  )
where

import Data.Char (ord)
import Data.Tree (flatten)
import Data.Time (Day, formatTime, defaultTimeLocale, iso8601DateFormat)
import qualified Data.Csv as C
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Wimm.Journal

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
writeReport :: FilePath -> Char -> Report -> IO ()
writeReport path c report =
  let myOptions = C.defaultEncodeOptions {
                      C.encDelimiter = fromIntegral (ord c)
                    }
  in BS.writeFile path $ BL.toStrict $ C.encodeWith myOptions report

-- | Formats a date for reporting
toISO8601 :: Day -> String
toISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

-- | A journal account with other informations that makes reporting easier
data RAccount = RAccount {
  rAccount :: Account,
  rAccountType :: AccountType,
  rIsEarningAccount :: Bool,
  rIsOpeningBalanceAccount :: Bool
}

rIdentifier :: RAccount -> Identifier
rIdentifier = aIdentifier . rAccount

rDisplayName :: RAccount -> T.Text
rDisplayName = aDisplayName . rAccount

rNumber :: RAccount -> Int
rNumber = aNumber . rAccount

rIsIncomeStatementType :: RAccount -> Bool
rIsIncomeStatementType = isIncomeStatementType . rAccountType

rIsBalanceSheetType :: RAccount -> Bool
rIsBalanceSheetType = isBalanceSheetType . rAccountType

-- | Returns a map from account identifier to account data type with extended
-- informations.
accountMap :: Journal -> HM.HashMap Identifier RAccount
accountMap journal = HM.fromList
               $ map (\a -> (rIdentifier a, a))
               $ concatMap (\(accType,f) -> map (toRAccount accType) $ flatten $ f journal)
               $ [(Asset, jAsset), 
                  (Liability, jLiability),
                  (Equity, jEquity),
                  (Revenue, jRevenue),
                  (Expense, jExpense)]

  where toRAccount :: AccountType -> Account -> RAccount
        toRAccount accType acc = 
          let isEarning = aIdentifier acc == (jEarningsAccount $ jReportParams journal)
              isOpenBal = aIdentifier acc == (jOpeningBalanceAccount $ jReportParams journal)
          in RAccount acc accType isEarning isOpenBal