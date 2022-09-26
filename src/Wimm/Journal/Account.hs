{-# LANGUAGE DeriveGeneric #-}

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
      aDisplayName,
      isBalanceSheetType,
      isIncomeStatementType,
      isCreditType,
      isDebitType,
      Identifier,
      Account(..)
    ) where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
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

type Identifier = T.Text

-- | The datatype to encode an account as it appears in the JSON journal files
data Account = Account {
  aIdentifier :: Identifier, -- Unique to each account. The identifier is used 
                         -- when other JSON object what to refer to this account.
  aDisplayNameM :: Maybe T.Text, -- Many accounts can have the same display name on the reports.
  aNumber :: Int -- Provided by the user. Must be different for each account. 
  -- For now let us use flat structure
  -- aParent :: T.Text -- Identifier of the parent, as defined in the account CSV files
}
  deriving (Eq, Show, Generic)

-- | Returns the provided display name or the identifier if none was provided
aDisplayName :: Account -> T.Text
aDisplayName (Account ident Nothing _) = ident
aDisplayName (Account _ (Just n) _) = n

instance ToJSON Account where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Account where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions{
  fieldLabelModifier = fieldName
}

fieldName :: String -> String
fieldName "aIdentifier" = "identifier"
fieldName "aDisplayNameM" = "name"
fieldName "aNumber" = "number"
fieldName x = x