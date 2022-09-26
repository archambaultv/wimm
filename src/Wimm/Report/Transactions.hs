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
  transactionReport
  )
where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Wimm.Report.Report
import Wimm.Journal

transactionReport :: ReportPeriod -> Journal -> Report
transactionReport (startD, endD) journal =
  let header = ["Id","Date","Account","Account number","Amount", "Counterparty","Comment","Tags"]
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
              ctp = tCounterParty txn
              comment = tComment txn
              tags = T.intercalate "|" $ tTags txn
              acc = pAccount p
              accNo = accNoMap HM.! acc
          in [no,date,acc,accNo,showAmount (jReportParams journal) (pAmount p),ctp,comment,tags] : report

        -- Key :: Account identifier
        -- Value :: Account number as text
        accNoMap :: HM.HashMap Identifier T.Text
        accNoMap = fmap (T.pack . show . rNumber) $ accountMap journal