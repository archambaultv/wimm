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

import Data.Functor.Foldable (cata)
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
        accMap :: HM.HashMap Identifier AccountInfo
        accMap = accInfoMap j

        -- Extract the postings from the journal, excluding future transactions
        -- and postings related to balance sheet accounts
        postings :: [Posting]
        postings = filter (isIncomeStatementType . aiAccountType . (accMap HM.!) . pAccount)
                 $ concatMap (map snd . txnToPostings)
                 $ filter (\t -> not (afterEndDate endD t || beforeStartDate startD t)) 
                 $ jTransactions j

        -- Serialize each account type tree
        revenueReport :: Report
        revenueReport = snd $ cata alg (jRevenue j)

        expenseReport :: Report
        expenseReport = snd $ cata alg (jExpense j)

        alg :: AccountF (Amount, Report) -> (Amount, Report)
        alg acc@(AccountF ident _ _ []) =
          let amnt = fromMaybe 0 (accountAmount ident)
              row = [aDisplayNameF acc,showAmount (jReportParams j) amnt]
          in (amnt, [row])
        alg acc@(AccountF ident _ _ children) =
          let childrenSum = sum $ map fst children
              topRow = [aDisplayNameF acc]
              amnt = (fromMaybe 0 (accountAmount ident)) + childrenSum
              childrenRow :: Report
              childrenRow = concatMap snd children
              finalRow = [aDisplayNameF acc,showAmount (jReportParams j) amnt]
          in (amnt, topRow : childrenRow ++ [finalRow])

        -- Helper functions
        balMap :: HM.HashMap Identifier Amount
        balMap = balanceMap postings

        accountAmount :: Identifier -> Maybe Amount
        accountAmount ident = HM.lookup ident balMap
