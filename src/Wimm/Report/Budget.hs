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

import Data.Tree (Tree(..), foldTree, flatten)
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
        budgetAcc :: [Tree Account]
        budgetAcc = budgetAccounts j budget

        budgetAccIdent :: [Identifier]
        budgetAccIdent = concatMap (map aIdentifier . flatten) budgetAcc

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
        budgetTree :: Tree Account
        budgetTree = Node (Account "Budget" "Budget" 0) budgetAcc

        budgetAccReport :: Report
        budgetAccReport = snd $ foldTree alg budgetTree

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

        mkReport :: (Journal -> Tree Account) -> Report
        mkReport f = 
            case removeBudgetAcc (f j) of
             Nothing -> []
             (Just t) -> snd $ foldTree alg t

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
        balMap :: HM.HashMap Identifier Amount
        balMap = balanceMap postings

        accountAmount :: Identifier -> Maybe Amount
        accountAmount ident = HM.lookup ident balMap

        removeBudgetAcc :: Tree Account -> Maybe (Tree Account)
        removeBudgetAcc = 
          let go :: Account -> [Maybe (Tree Account)] -> Maybe (Tree Account)
              go a xs = if aIdentifier a `elem` (bAccounts budget)
                        then Nothing
                        else Just (Node a (catMaybes xs))
          in foldTree go