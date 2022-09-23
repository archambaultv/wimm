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
      Posting(..)
    ) where

import Data.Time (Day)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   (.:?))
import qualified Data.Text as T
import Wimm.Journal.Amount

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data Posting = Posting
  {
    pBalanceDate :: Maybe Day, -- The date to consider when computing balance assertion
    pAccount :: T.Text, -- The account identifier
    pAmount :: Amount -- The amount
  } deriving (Eq, Show)

instance ToJSON Posting where
  toJSON (Posting balDate acc amount) =
        object $ ["Account" .= acc,
                  "Amount" .= toScientific amount] ++
                 (case balDate of {Nothing -> []; (Just b) -> ["Balance date" .= b]})
               
  toEncoding (Posting balDate acc amount) =
        pairs $ "Account" .= acc <>
                "Amount" .= toScientific amount <>
                (case balDate of {Nothing -> mempty; (Just b) -> "Balance date" .= b})
              
instance FromJSON Posting where
    parseJSON = withObject "Posting" $ \v -> Posting
      <$> (v .:? "Balance date")
      <*> v .: "Account"
      <*> fmap fromScientific (v .: "Amount")