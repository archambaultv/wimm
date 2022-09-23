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
import Wimm.Report.Report
import Wimm.Journal

transactionReport :: ReportPeriod -> Journal -> Report
transactionReport _ journal =
  let header = ["Id","Date","Account","Account number","Amount", "Counterparty","Comment"]
      numberedTxns = zip [1..] (jTransactions journal) :: [(Int, Transaction)]
  in header : foldr serializeTxn [] numberedTxns

  where serializeTxn ::  (Int, Transaction) -> Report -> Report
        serializeTxn (n, txn) report = 
          let no = T.pack $ show n
              date = T.pack $ toISO8601 $ tDate txn
              ctp = tCounterParty txn
              comment = tComment txn
          in foldr (serializePosting no date ctp comment) [] (tPostings txn) ++ report

        serializePosting :: T.Text -> T.Text -> T.Text -> T.Text -> Posting -> Report -> Report
        serializePosting no date ctp comment p report =
          [no,date,pAccount p,"",showAmount (jCurrency journal) (pAmount p),ctp,comment] : report