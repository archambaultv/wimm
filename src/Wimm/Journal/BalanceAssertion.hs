{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Wimm.Journal.BalanceAssertion
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what is a balance assertion.
module Wimm.Journal.BalanceAssertion (
  BalanceAssertion(..)
) where

import GHC.Generics
import Data.Time (Day)
import Wimm.Journal.Account
import Wimm.Journal.Amount
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs)

-- | Assert that at the end of the given day, the account must have the
-- specified balance
data BalanceAssertion = BalanceAssertion {
  baDate :: Day,
  baAccount :: Identifier,
  baAmount :: Amount
} deriving (Show, Eq, Generic)

instance ToJSON BalanceAssertion where
  toJSON (BalanceAssertion date acc amount) =
        object $ ["date" .= date,
                  "account" .= acc,
                  "amount" .= toScientific amount]
               
  toEncoding (BalanceAssertion date acc amount) =
        pairs $ "date" .= date <>
                "account" .= acc <>
                "amount" .= toScientific amount
              
instance FromJSON BalanceAssertion where
    parseJSON = withObject "Balance Assertion" $ \v -> BalanceAssertion
      <$> v .: "date"
      <*> v .: "account"
      <*> fmap fromScientific (v .: "amount")