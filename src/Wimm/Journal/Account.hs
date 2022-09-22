-- |
-- Module      :  Wimm.Journal.Account
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what is a account.
module Wimm.Journal.Account
    ( AccountType(..),
      isBalanceSheetType,
      isIncomeStatementType,
      isCreditType,
      isDebitType,
      Account(..),
      topAccounts
    ) where

import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import GHC.Generics (Generic)
import qualified Data.Text as T

-- | The top level grouping of an account. Must be Asset, Liability,
-- Equity, Revenue or Expense.
data AccountType = Asset
                | Liability
                | Equity
                | Revenue
                | Expense
                deriving (Show, Eq, Ord)

-- | Returns true if the this account type is in the balance sheet report
isBalanceSheetType :: AccountType -> Bool
isBalanceSheetType a = a `elem` [Asset, Liability, Equity]

-- | Returns true if the this account type is in the income statement report
isIncomeStatementType :: AccountType -> Bool
isIncomeStatementType = not . isBalanceSheetType

-- | Returns true if the this account type is considered a credit type. This is
-- usuaful for normalizing signs in the reports.
isCreditType :: AccountType -> Bool
isCreditType a = a `elem` [Liability, Equity, Revenue]

-- | Returns true if the this account type is considered a debit type. This is
-- usuaful for normalizing signs in the reports.
isDebitType :: AccountType -> Bool
isDebitType = not . isCreditType

-- | The datatype to encode an account as it appears in the JSON journal files
data Account = Account {
  aIdentifier :: T.Text, -- Unique to each account. The identifier is used 
                         -- when other JSON object what to refer to this account.
  aDisplayName :: T.Text, -- Many accounts can have the same display name on the reports.
  aNumber :: Int -- Provided by the user. Must be different for each account. As a rule, it must respec
  -- For now let us use flat structure
  -- aParent :: T.Text -- Identifier of the parent, as defined in the account CSV files
}
  deriving (Generic, Show)

instance Eq Account where
  (==) a1 a2 = aNumber a1 == aNumber a2

instance ToJSON Account where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Account

topAccounts :: (Account, Account, Account, Account, Account)
topAccounts =
  (Account "Actif" "Actif" 1000,
   Account "Passif" "Passif" 2000,
   Account "Capital" "Capital" 3000,
   Account "Revenus" "Revenus" 4000,
   Account "Dépense" "Dépense" 5000)