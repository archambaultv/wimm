-- |
-- Module      :  Wimm.Report.IncomeStatement
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the balanche sheet report

module Wimm.Report.IncomeStatement
(
  incomeStatementReport
  )
where

import Data.Tree (foldTree)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Wimm.Report.Report
import Wimm.Journal

-- | Balance sheet report
incomeStatementReport :: ReportPeriod -> Journal -> Report
incomeStatementReport (startD, endD) j = concat [revenueReport, expenseReport]
  where
        -- Build the account map so we can associate an Identifier to all the
        -- information regarding the account
        accMap :: HM.HashMap Identifier RAccount
        accMap = accountMap j

        -- Extract the postings from the journal, excluding future transactions
        -- and postings related to balance sheet accounts
        postings :: [Posting]
        postings = filter (rIsIncomeStatementType . (accMap HM.!) . pAccount)
                 $ concatMap (map snd . txnToPostings)
                 $ filter (\t -> not (afterEndDate t || beforeStartDate t)) 
                 $ jTransactions j

        -- Serialize each account type tree
        revenueReport :: Report
        revenueReport = snd $ foldTree alg (jRevenue j)

        expenseReport :: Report
        expenseReport = snd $ foldTree alg (jExpense j)

        alg :: Account -> [(Amount, Report)] -> (Amount, Report)
        alg acc [] =
          let ident = aIdentifier acc
              amnt = fromMaybe 0 (accountAmount ident)
              row = [aDisplayName acc,showAmount (jReportParams j) amnt]
          in (amnt, [row])
        alg acc children =
          let childrenSum = sum $ map fst children
              topRow = [aDisplayName acc]
              ident = aIdentifier acc
              amnt = (fromMaybe 0 (accountAmount ident)) + childrenSum
              childrenRow :: Report
              childrenRow = concatMap snd children
              finalRow = [aDisplayName acc,showAmount (jReportParams j) amnt]
          in (amnt, topRow : childrenRow ++ [finalRow])

        -- Helper functions
        afterEndDate :: Transaction -> Bool
        afterEndDate t = case endD of
            Nothing -> False
            (Just d) -> tDate t > d

        beforeStartDate :: Transaction -> Bool
        beforeStartDate t = case startD of
            Nothing -> False
            (Just d) -> tDate t < d

        balanceMap :: HM.HashMap Identifier Amount
        balanceMap = HM.fromListWith (+) 
                   $ map (\p -> (pAccount p, pAmount p)) postings

        accountAmount :: Identifier -> Maybe Amount
        accountAmount ident = HM.lookup ident balanceMap
