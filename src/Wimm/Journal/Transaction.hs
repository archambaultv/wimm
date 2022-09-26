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
      Transaction(..),
      txnToPostings
    ) where

import Data.Time (Day)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   (.:?), (.!=))
import qualified Data.Text as T
import Wimm.Journal.Posting

-- | A transaction is a transfert of funds between some accounts.
data Transaction = Transaction
  {
    tDate :: Day,
    tCounterParty :: T.Text,
    tTags :: [T.Text],
    tPostings :: [Posting],
    tComment :: T.Text,
    tStatementDescription :: T.Text -- This field is use to deduplicate transactions on import
  } deriving (Eq, Show)

instance ToJSON Transaction where
  toJSON (Transaction date cc tags postings comment desc) =
        object $ ["Date" .= date, 
                "Postings" .= postings] ++
                (if T.null cc then [] else ["Counterparty" .= cc]) ++
                (if null tags then [] else ["Tags" .= tags]) ++
                (if T.null comment then [] else ["Comment" .= comment]) ++
                (if T.null desc then [] else ["Statement description" .= desc])
  toEncoding (Transaction date cc tags postings comment desc) =
        pairs $ "Date" .= date <>
                "Postings" .= postings <>
                (if T.null cc then mempty else "Counterparty" .= cc) <>
                (if null tags then mempty else "Tags" .= tags) <>
                (if T.null comment then mempty else "Comment" .= comment) <>
                (if T.null desc then mempty else "Statement description" .= desc)
              
instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \v -> Transaction
        <$> v .: "Date"
        <*> (v .:? "Counterparty" .!= "")
        <*> (v .:? "Tags" .!= [])
        <*> v .: "Postings"
        <*> (v .:? "Comment" .!= "")
        <*> (v .:? "Statement description" .!= "")

txnToPostings :: Transaction -> [(Transaction, Posting)]
txnToPostings t = map (t,) $ tPostings t