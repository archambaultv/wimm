-- |
-- Module      :  Wimm.Report.Transactions
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the transactions report

module Wimm.Report.Transactions
(
  TxnReportParams(..),
  transactionReport
  )
where

import Data.Time (Day)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Wimm.Report.Report
import Wimm.Journal

data TxnReportParams = TxnReportParams {
  trpStartDate :: Maybe Day,
  trpEndDate :: Maybe Day,
  trpExtraInfo :: Bool
}

-- | Exports transactions    
transactionReport :: TxnReportParams -> Journal -> Report
transactionReport (TxnReportParams startD endD extraInfo) journal =
  let extraHeader = if extraInfo then ["Account Type","Account Number","Account name"] else []
      header = ["Id","Date","Account","Amount","Balance date", "Counterparty","Comment","Tags","Statement description"]
             ++ extraHeader
      txns = filter (\t -> not (afterEndDate endD t || beforeStartDate startD t)) 
           $ jTransactions journal
      numberedTxns = zip [1..] txns :: [(Int, Transaction)]
  in header : foldr serializeTxn [] numberedTxns

  where serializeTxn ::  (Int, Transaction) -> Report -> Report
        serializeTxn t report = foldr (serializePosting t) [] (tPostings (snd t)) 
                              ++ report

        serializePosting :: (Int, Transaction) -> Posting -> Report -> Report
        serializePosting (n, txn) p report =
          let no = T.pack $ show n
              date = T.pack $ toISO8601 $ tDate txn
              balDate = case pBalanceDate p of
                          Nothing -> ""
                          (Just d) -> T.pack $ toISO8601 $ d
              ctp = tCounterParty txn
              comment = tComment txn
              tags = T.intercalate "|" $ tTags txn
              acc = pAccount p
              m = accNoMap HM.! acc
              extraData = if extraInfo
                          then [T.pack $ show (aiAccountType m),
                                T.pack $ show (aiNumber m),
                                aiDisplayName m]
                          else []
          in ([no,date,acc,showAmount (jReportParams journal) (pAmount p),balDate,ctp,comment,tags,(tStatementDescription txn)]
             ++ extraData) : report

        -- Key :: Account identifier
        -- Value :: Account number as text
        accNoMap :: HM.HashMap Identifier AccountInfo
        accNoMap = accInfoMap journal