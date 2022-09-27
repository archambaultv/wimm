{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, TemplateHaskell, TypeFamilies #-}

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
      aDisplayNameF,
      isBalanceSheetType,
      isIncomeStatementType,
      isCreditType,
      isDebitType,
      Identifier,
      Account(..),
      AccountF(..),
      flattenIdentifier,
    ) where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), 
                   object, (.=), withObject, (.:), pairs, (.:?), (.!=))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable (cata)
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
  aNumber :: Int, -- Provided by the user. Must be different for each account.
  aSubAccounts :: [Account] -- The sub accounts of this account
  -- For now let us use flat structure
  -- aParent :: T.Text -- Identifier of the parent, as defined in the account CSV files
}
  deriving (Eq, Show, Generic)

makeBaseFunctor ''Account

-- | Returns the provided display name or the identifier if none was provided
aDisplayName :: Account -> T.Text
aDisplayName (Account ident Nothing _ _) = ident
aDisplayName (Account _ (Just n) _ _) = n

aDisplayNameF :: AccountF a -> T.Text
aDisplayNameF (AccountF ident Nothing _ _) = ident
aDisplayNameF (AccountF _ (Just n) _ _) = n

flattenIdentifier :: Account -> [Identifier]
flattenIdentifier = cata alg
  where alg :: AccountF [Identifier] -> [Identifier]
        alg (AccountF ident _ _ xs) = ident : concat xs

-- instance ToJSON Account where
--   toJSON = genericToJSON customOptions
--   toEncoding = genericToEncoding customOptions

-- instance FromJSON Account where
--   parseJSON = genericParseJSON customOptions

-- customOptions :: Options
-- customOptions = defaultOptions{
--   fieldLabelModifier = fieldName,
--   omitNothingFields = True
-- }

instance ToJSON Account where
  toJSON (Account ident name number subAccs) =
        object $ ["identifier" .= ident,
                  "number" .= number] ++
                 (case name of {Nothing -> []; (Just n) -> ["name" .= n]}) ++
                 (if null subAccs then [] else ["sub accounts" .= subAccs])
  toEncoding (Account ident name number subAccs) =
        pairs $ "identifier" .= ident <>
                "number" .= number <>
                (case name of {Nothing -> mempty; (Just n) -> "name" .= n}) <>
                (if null subAccs then mempty else "sub accounts" .= subAccs)
              
instance FromJSON Account where
    parseJSON = withObject "Account" $ \v -> Account
      <$> v .: "identifier"
      <*> (v .:? "name")
      <*> v .: "number"
      <*> (v .:? "sub accounts" .!= [])