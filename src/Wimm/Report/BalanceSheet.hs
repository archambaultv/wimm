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

import Data.Functor.Foldable (cata)
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
        accMap :: HM.HashMap Identifier AccountInfo
        accMap = accInfoMap j

        -- Extract the postings from the journal, excluding future transactions
        -- and postings related to income statement accounts
        postings :: [(Transaction, Posting)]
        postings = filter (isBalanceSheetType . aiAccountType . (accMap HM.!) . pAccount . snd)
                 $ concatMap txnToPostings
                 $ filter (not . afterEndDate endD) 
                 $ jTransactions j

        -- Compute opening balance and earnings
        openBalAmnt = negate
                    $ sum
                    $ map (\(t,p) -> if beforeStartDate startD t then pAmount p else 0)
                      postings       

        earningAmnt = negate
                    $ sum
                    $ map (\(t,p) -> if beforeStartDate endD t then 0 else pAmount p)
                      postings 

        -- Serialize each account type tree
        assetReport :: Report
        assetReport = snd $ cata alg (jAsset j)

        liabilityReport :: Report
        liabilityReport = snd $ cata alg (jLiability j)

        equityReport :: Report
        equityReport = snd $ cata alg (jEquity j)

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
        balMap = balanceMap (map snd postings)

        accountAmount :: Identifier -> Maybe Amount
        accountAmount ident | ident == (jOpeningBalanceAccount j) =
          case (HM.lookup ident balMap) of
            Nothing -> Just openBalAmnt
            (Just x) -> Just (x + openBalAmnt)
        accountAmount ident | ident == (jEarningsAccount j) =
          case (HM.lookup ident balMap) of
            Nothing -> Just earningAmnt
            (Just x) -> Just (x + earningAmnt)
        accountAmount ident = HM.lookup ident balMap


