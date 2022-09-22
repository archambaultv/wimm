-- |
-- Module      :  Wimm.Journal.Transaction
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what a transaction is.
module Wimm.Journal.Transaction
    ( 
      Transaction(..)
    ) where

import Data.Time (Day)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   (.:?), (.!=))
import qualified Data.Text as T
import Wimm.Journal.Posting

data Transaction = Transaction
  {
    tDate :: Day,
    tCounterParty :: T.Text,
    tTags :: [T.Text],
    tPostings :: [Posting]
  } deriving (Eq, Show)

instance ToJSON Transaction where
  toJSON (Transaction date cc tags postings) =
        object $ ["Date" .= date, 
                "Postings" .= postings] ++
                (if T.null cc then [] else ["Counterparty" .= cc]) ++
                (if null tags then [] else ["Tags" .= tags])
  toEncoding (Transaction date cc tags postings) =
        pairs $ "Date" .= date <>
                "Postings" .= postings <>
                (if T.null cc then mempty else "Counterparty" .= cc) <>
                (if null tags then mempty else "Tags" .= tags)
              
instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \v -> Transaction
        <$> v .: "Date"
        <*> (v .:? "Counterparty" .!= "")
        <*> (v .:? "Tags" .!= [])
        <*> v .: "Postings"