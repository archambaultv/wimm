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
      beforeStartDate,
      removeDuplicateTxns
    ) where

import Data.Hashable
import Data.Scientific (Scientific)
import Data.Time (Day)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Wimm.Journal.Posting
import Wimm.Journal.Amount
import Wimm.Journal.Account (Identifier)

-- | A transaction is a transfert of funds between some accounts.
data Transaction = Transaction
  {
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

-- | The part of a transaction that must match for a duplicate detection
data TxnKey = TxnKey {
  tkDate :: Day,
  tkPostings :: [(Identifier, Scientific)],
  tkStatementDesc :: T.Text
} deriving (Eq, Ord, Show, Generic)

instance Hashable TxnKey

toTxnKey :: Transaction -> TxnKey
toTxnKey t = TxnKey (tDate t) 
                    (map (\p -> (pAccount p,toScientific $ pAmount p)) $ tPostings t) 
                    (tStatementDescription t)

-- | removeDuplicateTxns acc old new returns new', the new transactions where
-- the existing one in old are filtered out
removeDuplicateTxns :: [Transaction] -> 
                       [Transaction] -> 
                       [Transaction]
removeDuplicateTxns old new =
  let oldKeys :: HM.HashMap TxnKey Int
      oldKeys = HM.fromListWith (+) 
              $ zip (map toTxnKey old) (repeat 1)
      dedup :: Transaction -> (HM.HashMap TxnKey Int, [Transaction]) -> (HM.HashMap TxnKey Int, [Transaction])
      dedup t (m, acc) =
        let k = (toTxnKey t)
            foo :: (Maybe Int -> Maybe (Maybe Int))
            foo Nothing = Nothing -- Simply not in the map
            foo (Just 1) = (Just Nothing) -- Remove the key
            foo (Just n) = (Just (Just (n - 1))) -- Decrement the value
        in case HM.alterF foo k m of
              Nothing -> (m, t : acc) -- Not in old
              Just x -> (x, acc) -- In old

  in snd $ foldr dedup (oldKeys, []) new


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
fieldName "tDate" = "date"
fieldName "tCounterPartyM" = "counterparty"
fieldName "tTagsM" = "tags"
fieldName "tPostings" = "postings"
fieldName "tCommentM" = "comment"
fieldName "tStatementDescriptionM" = "statement description"
fieldName x = x