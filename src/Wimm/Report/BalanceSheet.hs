-- |
-- Module      :  Wimm.Report.BalanceSheet
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the balanche sheet report

module Wimm.Report.BalanceSheet
(
  balanceSheetReport
  )
where

import Data.Tree (foldTree)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Wimm.Report.Report
import Wimm.Journal

-- | Balance sheet report
balanceSheetReport :: ReportPeriod -> Journal -> Report
balanceSheetReport (startD, endD) j = concat [assetReport, liabilityReport, equityReport]
  where
        -- Build the account map so we can associate an Identifier to all the
        -- information regarding the account
        accMap :: HM.HashMap Identifier RAccount
        accMap = accountMap j

        -- Extract the postings from the journal, excluding future transactions
        -- and postings related to income statement accounts
        postings :: [(Transaction, Posting)]
        postings = filter (rIsBalanceSheetType . (accMap HM.!) . pAccount . snd)
                 $ concatMap txnToPostings
                 $ filter (not . afterEndDate) 
                 $ jTransactions j

        -- Compute opening balance and earnings
        openBalAmnt = negate
                    $ sum
                    $ map (\(t,p) -> if beforeStartDate t then pAmount p else 0)
                      postings       

        earningAmnt = negate
                    $ sum
                    $ map (\(t,p) -> if beforeStartDate t then 0 else pAmount p)
                      postings 

        -- Serialize each account type tree
        assetReport :: Report
        assetReport = snd $ foldTree alg (jAsset j)

        liabilityReport :: Report
        liabilityReport = snd $ foldTree alg (jLiability j)

        equityReport :: Report
        equityReport = snd $ foldTree alg (jEquity j)

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
                   $ map ((\p -> (pAccount p, pAmount p)) . snd) postings

        accountAmount :: Identifier -> Maybe Amount
        accountAmount ident | ident == (jOpeningBalanceAccount j) =
          case (HM.lookup ident balanceMap) of
            Nothing -> Just openBalAmnt
            (Just x) -> Just (x + openBalAmnt)
        accountAmount ident | ident == (jEarningsAccount j) =
          case (HM.lookup ident balanceMap) of
            Nothing -> Just earningAmnt
            (Just x) -> Just (x + earningAmnt)
        accountAmount ident = HM.lookup ident balanceMap


