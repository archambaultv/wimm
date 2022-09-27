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
      budgetAccounts,
      accountForest,
      AccountInfo(..),
      accInfoMap
    ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Functor.Foldable (cata, para)
import GHC.Generics
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
  jAsset :: Account,
  jLiability :: Account,
  jEquity :: Account,
  jRevenue :: Account,
  jExpense :: Account,

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
fieldName "jAsset" = "assets account tree"
fieldName "jLiability" = "liabilities account tree"
fieldName "jEquity" = "equity account tree"
fieldName "jRevenue" = "revenue account tree"
fieldName "jExpense" = "expenses account tree"
fieldName "jTransactions" = "transactions"
fieldName "jDefaultBudget" = "default budget"
fieldName "jBudgets" = "budgets"
fieldName x = x

-- | The five top accounts
accountForest :: Journal -> [Account]
accountForest j = [jAsset j, jLiability j, jEquity j, jRevenue j, jExpense j]

-- | Returns the list of all budget accounts (and their children)
-- Does not return an account twice if the parent is also in the list of budget accounts
budgetAccounts :: Journal -> Budget -> [Account]
budgetAccounts j budget = concatMap (para alg)
                        $ accountForest j
  
  where alg :: AccountF (Account, [Account]) -> [Account]
        alg (AccountF ident name number xs) =
          if ident `elem` bAccs
          then [Account ident name number (map fst xs)]
          else concatMap snd xs

        bAccs :: [Identifier]
        bAccs = bAccounts budget

-- | The information associated with an identifier in the journal file
data AccountInfo = AccountInfo {
  aiIdentifier :: T.Text,
  aiAccountType :: AccountType,
  aiNumber :: Int,
  aiDisplayName :: T.Text
}
-- | Returns a map from Identifier to account type
accInfoMap :: Journal -> HM.HashMap Identifier AccountInfo
accInfoMap j = HM.fromList
             $ concatMap (\(accType, f) -> cata (alg accType) (f j))
              [(Asset, jAsset), 
              (Liability, jLiability),
              (Equity, jEquity),
              (Revenue, jRevenue),
              (Expense, jExpense)]
    where alg :: AccountType -> AccountF [(Identifier, AccountInfo)] -> [(Identifier, AccountInfo)]
          alg accType acc@(AccountF ident _ number xs) = 
            (ident, AccountInfo ident accType number (aDisplayNameF acc))
                : concat xs