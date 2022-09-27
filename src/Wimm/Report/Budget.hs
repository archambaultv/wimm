-- |
-- Module      :  Wimm.Report.Budget
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the budget report

module Wimm.Report.Budget
(
  budgetReport
  )
where

import Data.Functor.Foldable (cata)
import Data.Maybe (fromMaybe,catMaybes)
import qualified Data.HashMap.Strict as HM
import Wimm.Report.Report
import Wimm.Journal

-- | Balance sheet report
budgetReport :: ReportPeriod -> Journal -> Budget -> Report
budgetReport (startD, endD) j budget = 
  concat [budgetAccReport, assetReport, liabilityReport, 
          equityReport, revenueReport, expenseReport]
  where
        -- Any transactions that contains at least one posting in the budgets accounts
        -- is considered a transaction affecting the budget. Any children of a budget
        -- account is also in the budget
        budgetAcc :: [Account]
        budgetAcc = budgetAccounts j budget

        budgetAccIdent :: [Identifier]
        budgetAccIdent = concatMap flattenIdentifier budgetAcc

        -- Extract the postings from the journal, excluding out of period
        -- transactions and out of budget transactions
        postings :: [Posting]
        postings = concatMap (map snd . txnToPostings)
                 $ filter (\t ->    not (afterEndDate endD t || beforeStartDate startD t) 
                                 && txnInBudget t) 
                 $ jTransactions j

        txnInBudget ::  Transaction -> Bool
        txnInBudget = any (\p -> pAccount p `elem` budgetAccIdent)
                    . tPostings

        -- Serialize each account type tree and a special one for the budget
        budgetTree :: Account
        budgetTree = Account "Budget" Nothing 0 budgetAcc

        budgetAccReport :: Report
        budgetAccReport = snd $ cata alg budgetTree

        assetReport :: Report
        assetReport = mkReport jAsset

        liabilityReport :: Report
        liabilityReport = mkReport jLiability

        equityReport :: Report
        equityReport = mkReport jEquity

        revenueReport :: Report
        revenueReport = mkReport jRevenue

        expenseReport :: Report
        expenseReport = mkReport jExpense

        mkReport :: (Journal -> Account) -> Report
        mkReport f = 
            case removeBudgetAcc (f j) of
             Nothing -> []
             (Just t) -> snd $ cata alg t

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

        -- | Remove the budget accounts from
        removeBudgetAcc :: Account -> Maybe (Account)
        removeBudgetAcc = cata alg1
          where alg1 :: AccountF (Maybe Account) -> Maybe Account
                alg1 (AccountF ident name n xs) =
                  if ident `elem` (bAccounts budget)
                  then Nothing
                  else Just (Account ident name n (catMaybes xs))