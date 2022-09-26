-- |
-- Module      :  Wimm.Journal.Posting
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what a posting is.
module Wimm.Journal.Posting
    ( 
      Posting(..),
      balanceMap
    ) where

import Data.Time (Day)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   (.:?))
import qualified Data.HashMap.Strict as HM
import Wimm.Journal.Amount
import Wimm.Journal.Account

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data Posting = Posting
  {
    pBalanceDate :: Maybe Day, -- The date to consider when computing balance assertion
    pAccount :: Identifier, -- The account identifier
    pAmount :: Amount -- The amount
  } deriving (Eq, Show)

balanceMap :: [Posting] -> HM.HashMap Identifier Amount
balanceMap = HM.fromListWith (+) 
           . map (\p -> (pAccount p, pAmount p))

instance ToJSON Posting where
  toJSON (Posting balDate acc amount) =
        object $ ["account" .= acc,
                  "amount" .= toScientific amount] ++
                 (case balDate of {Nothing -> []; (Just b) -> ["balance date" .= b]})
               
  toEncoding (Posting balDate acc amount) =
        pairs $ "account" .= acc <>
                "amount" .= toScientific amount <>
                (case balDate of {Nothing -> mempty; (Just b) -> "balance date" .= b})
              
instance FromJSON Posting where
    parseJSON = withObject "Posting" $ \v -> Posting
      <$> (v .:? "balance date")
      <*> v .: "account"
      <*> fmap fromScientific (v .: "amount")