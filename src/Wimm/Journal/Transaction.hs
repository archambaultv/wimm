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
import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Wimm.Journal.Posting

data Transaction = Transaction
  {
    tDate :: Day,
    tCounterParty :: T.Text,
    tTags :: [T.Text],
    tPostings :: [Posting]
  } deriving (Eq, Show, Generic)

instance ToJSON Transaction where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Transaction