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
    ( Journal(..)
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs)
import qualified Data.Text as T
import Wimm.Journal.Account
import Wimm.Journal.Currency
import Wimm.Journal.Transaction

-- | The Journal is a file that contains all the financial data (transactions)
-- and other info like account descriptions needed to process the data
data Journal = Journal {
   -- | The account in the balance sheet that we must use as the opening balance account.
   jOpeningBalanceAccount :: T.Text,
   
   -- | The account in the balance sheet that we must use as the earnings account.
   jEarningsAccount :: T.Text,
   
   -- | The name of the company or the name to display in the reports
   jCompanyName :: T.Text,

    -- | First month of the fiscal year
   jFirstFiscalMonth :: Int,

   -- | Currency description
   jCurrency :: Currency,

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
  toJSON (Journal open earn comp ffm curr asset lia equi rev expe txns) =
        object ["Opening balance account" .= open, 
                "Earnings account" .= earn,
                "Company name" .= comp,
                "First fiscal month" .= ffm,
                "Currency" .= curr,
                "Asset accounts" .= asset,
                "Liability accounts" .= lia,
                "Equity accounts" .= equi,
                "Revenue accounts" .= rev,
                "Expense accounts" .= expe,
                "Transactions" .= txns]
  toEncoding (Journal open earn comp ffm curr asset lia equi rev expe txns) =
        pairs ( "Opening balance account" .= open <>
                "Earnings account" .= earn <>
                "Company name" .= comp <>
                "First fiscal month" .= ffm <>
                "Currency" .= curr <>
                "Asset accounts" .= asset <>
                "Liability accounts" .= lia <>
                "Equity accounts" .= equi <>
                "Revenue accounts" .= rev <>
                "Expense accounts" .= expe <>
                "Transactions" .= txns)

instance FromJSON Journal where
    parseJSON = withObject "Journal" $ \v -> Journal
        <$> v .: "Opening balance account"
        <*> v .: "Earnings account"
        <*> v .: "Company name"
        <*> v .: "First fiscal month"
        <*> v .: "Currency"
        <*> v .: "Asset accounts"
        <*> v .: "Liability accounts"
        <*> v .: "Equity accounts"
        <*> v .: "Revenue accounts"
        <*> v .: "Expense accounts"
        <*> v .: "Transactions"