{-# LANGUAGE DeriveGeneric #-}

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
      tCounterParty,
      tTags,
      tComment,
      tStatementDescription,
      txnToPostings,
      afterEndDate,
      beforeStartDate
    ) where

import Data.Time (Day)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
import qualified Data.Text as T
import Wimm.Journal.Posting


-- | A transaction is a transfert of funds between some accounts.
data Transaction = Transaction
  {
    tId :: Int,
    tDate :: Day,
    tCounterPartyM :: Maybe T.Text,
    tTagsM :: Maybe [T.Text],
    tPostings :: [Posting],
    tCommentM :: Maybe T.Text,
    tStatementDescriptionM :: Maybe T.Text -- This field is use to deduplicate transactions on import
  } deriving (Eq, Show, Generic)

tCounterParty :: Transaction -> T.Text
tCounterParty t = fromMaybe "" $ tCounterPartyM t

tTags :: Transaction -> [T.Text]
tTags t = fromMaybe [] $ tTagsM t

tComment :: Transaction -> T.Text
tComment t = fromMaybe "" $ tCommentM t

tStatementDescription :: Transaction -> T.Text
tStatementDescription t = fromMaybe "" $ tStatementDescriptionM t

afterEndDate :: Maybe Day -> Transaction -> Bool
afterEndDate endD t = case endD of
    Nothing -> False
    (Just d) -> tDate t > d

beforeStartDate :: Maybe Day -> Transaction -> Bool
beforeStartDate startD t = case startD of
    Nothing -> False
    (Just d) -> tDate t < d

txnToPostings :: Transaction -> [(Transaction, Posting)]
txnToPostings t = map (t,) $ tPostings t


instance ToJSON Transaction where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Transaction where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions{
  fieldLabelModifier = fieldName,
  omitNothingFields = True
}

fieldName :: String -> String
fieldName "tId" = "id"
fieldName "tDate" = "date"
fieldName "tCounterPartyM" = "counterparty"
fieldName "tTagsM" = "tags"
fieldName "tPostings" = "postings"
fieldName "tCommentM" = "comment"
fieldName "tStatementDescriptionM" = "statement description"
fieldName x = x