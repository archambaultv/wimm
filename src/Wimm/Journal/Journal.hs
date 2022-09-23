-- |
-- Module      :  Wimm.Journal.Journal
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what is a journal, the file read by wimm. 

module Wimm.Journal.Journal
    ( Journal(..),
      journalAccounts
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs)
import Wimm.Journal.Account
import Wimm.Journal.ReportParameters
import Wimm.Journal.Transaction

-- | The Journal is a file that contains all the financial data (transactions)
-- and other info like account descriptions needed to process the data
data Journal = Journal {
  -- | The defaults parameters for reporting
  jReportParams :: JournalReportParameters,

  -- | The accounts. For now we use a flat structure
  jAsset :: [Account],
  jLiability :: [Account],
  jEquity :: [Account],
  jRevenue :: [Account],
  jExpense :: [Account],

  -- | The transactions.
  jTransactions :: [Transaction]
  }
  deriving (Eq, Show)

instance ToJSON Journal where
  toJSON (Journal reportParams asset lia equi rev expe txns) =
        object ["Report parameters" .= reportParams, 
                "Asset accounts" .= asset,
                "Liability accounts" .= lia,
                "Equity accounts" .= equi,
                "Revenue accounts" .= rev,
                "Expense accounts" .= expe,
                "Transactions" .= txns]
  toEncoding (Journal reportParams  asset lia equi rev expe txns) =
        pairs ( "Report parameters" .= reportParams <>
                "Asset accounts" .= asset <>
                "Liability accounts" .= lia <>
                "Equity accounts" .= equi <>
                "Revenue accounts" .= rev <>
                "Expense accounts" .= expe <>
                "Transactions" .= txns)

instance FromJSON Journal where
    parseJSON = withObject "Journal" $ \v -> Journal
        <$> v .: "Report parameters"
        <*> v .: "Asset accounts"
        <*> v .: "Liability accounts"
        <*> v .: "Equity accounts"
        <*> v .: "Revenue accounts"
        <*> v .: "Expense accounts"
        <*> v .: "Transactions"

-- | Returns the list of all accounts
journalAccounts :: Journal -> [Account]
journalAccounts j = jAsset j ++ jLiability j ++ jEquity j ++ jRevenue j ++ jExpense j