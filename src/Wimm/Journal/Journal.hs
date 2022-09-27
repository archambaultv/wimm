{-# LANGUAGE DeriveGeneric #-}

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
      incomeStatementAccounts,
      budgetAccounts,
      accountForest
    ) where

import qualified Data.Text as T
import GHC.Generics
import Data.Tree (Tree(..), flatten, foldTree)
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
import Wimm.Journal.Account
import Wimm.Journal.ReportParameters
import Wimm.Journal.Transaction
import Wimm.Journal.Budget

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
  jTransactions :: [Transaction],

  -- | The default budget
  jDefaultBudget :: T.Text,

  -- | The budgets
  jBudgets :: [Budget]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Journal where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Journal where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions{
  fieldLabelModifier = fieldName,
  omitNothingFields = True
}

fieldName :: String -> String
fieldName "jOpeningBalanceAccount" = "opening balance account"
fieldName "jEarningsAccount" = "earnings account"
fieldName "jCompanyName" = "company name"
fieldName "jFirstFiscalMonth" = "first fiscal month"
fieldName "jReportParams" = "csv report parameters"
fieldName "jAsset" = "asset accounts"
fieldName "jLiability" = "liability accounts"
fieldName "jEquity" = "equity accounts"
fieldName "jRevenue" = "revenue accounts"
fieldName "jExpense" = "expense accounts"
fieldName "jTransactions" = "transactions"
fieldName "jDefaultBudget" = "default budget"
fieldName "jBudgets" = "budgets"
fieldName x = x

-- | Returns the list of all accounts that appears on the income statement
incomeStatementAccounts :: Journal -> [Account]
incomeStatementAccounts j = concatMap go [jRevenue, jExpense]
  where go :: (Journal -> Tree Account) -> [Account]
        go f = flatten $ f j

-- | The five account tree
accountForest :: Journal -> [Tree Account]
accountForest j = [jAsset j, jLiability j, jEquity j, jRevenue j, jExpense j]

-- | Returns the list of all budget accounts tree so they include their children
budgetAccounts :: Journal -> Budget -> [Tree Account]
budgetAccounts j budget = concatMap (\x -> foldTree alg x False)
                        $ accountForest j
  
  where alg :: Account -> [Bool -> [Tree Account]] -> (Bool -> [Tree Account])
        alg acc xs parent =
          if parent || aIdentifier acc `elem` bAccs
          then [Node acc (concatMap (\f -> f True) xs)]
          else concatMap (\f -> f False) xs

        bAccs :: [Identifier]
        bAccs = bAccounts budget