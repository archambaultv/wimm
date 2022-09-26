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
      incomeStatementAccounts
    ) where

import qualified Data.Text as T
import Data.Tree (Tree, flatten)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,(.:?), (.!=))
import Wimm.Journal.Account
import Wimm.Journal.ReportParameters
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

  -- | The defaults parameters for reporting
  jReportParams :: JournalReportParameters,

  -- | The accounts. One tree per account type
  jAsset :: Tree Account,
  jLiability :: Tree Account,
  jEquity :: Tree Account,
  jRevenue :: Tree Account,
  jExpense :: Tree Account,

  -- | The transactions.
  jTransactions :: [Transaction]
  }
  deriving (Eq, Show)

instance ToJSON Journal where
  toJSON (Journal open earn comp ffm reportParams asset lia equi rev expe txns) =
        object $ (if T.null comp then [] else ["Company name" .= comp]) ++
               ["Opening balance account" .= open, 
                "Earnings account" .= earn,
                "First fiscal month" .= ffm,
                "Csv Report parameters" .= reportParams, 
                "Asset accounts" .= asset,
                "Liability accounts" .= lia,
                "Equity accounts" .= equi,
                "Revenue accounts" .= rev,
                "Expense accounts" .= expe,
                "Transactions" .= txns]
  toEncoding (Journal open earn comp ffm reportParams  asset lia equi rev expe txns) =
        pairs $ (if T.null comp then mempty else "Company name" .= comp) <>
                "Opening balance account" .= open <>
                "Earnings account" .= earn <>
                "First fiscal month" .= ffm <>
                "Csv Report parameters" .= reportParams <>
                "Asset accounts" .= asset <>
                "Liability accounts" .= lia <>
                "Equity accounts" .= equi <>
                "Revenue accounts" .= rev <>
                "Expense accounts" .= expe <>
                "Transactions" .= txns

instance FromJSON Journal where
    parseJSON = withObject "Journal" $ \v -> Journal
        <$> v .: "Opening balance account"
        <*> v .: "Earnings account"
        <*> v .:? "Company name" .!= ""
        <*> v .:? "First fiscal month" .!= 1
        <*> v .: "Csv Report parameters"
        <*> v .: "Asset accounts"
        <*> v .: "Liability accounts"
        <*> v .: "Equity accounts"
        <*> v .: "Revenue accounts"
        <*> v .: "Expense accounts"
        <*> v .: "Transactions"

-- | Returns the list of all accounts that appears on the income statement
incomeStatementAccounts :: Journal -> [Account]
incomeStatementAccounts j = concatMap go [jRevenue, jExpense]
  where go :: (Journal -> Tree Account) -> [Account]
        go f = flatten $ f j