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
import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import GHC.Generics (Generic)
import qualified Data.Text as T

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data Posting = Posting
  {
    pBalanceDate :: Maybe Day, -- The date to consider when computing balance assertion
    pAccount :: T.Text, -- The account identifier
    pAmount :: Int -- The amount
  } deriving (Eq, Show, Generic)

instance ToJSON Posting where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Posting