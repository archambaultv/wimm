{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Wimm.Journal.Budget
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what a budget is.

module Wimm.Journal.Budget
    ( Budget(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
import qualified Data.Text as T
import Wimm.Journal.Account

-- | A budget
data Budget = Budget {
  bName :: T.Text,
  bAccounts :: [Identifier]
} deriving (Eq, Show, Generic)

instance ToJSON Budget where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Budget where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions{
  fieldLabelModifier = fieldName
}

fieldName :: String -> String
fieldName "bName" = "Name"
fieldName "bAccounts" = "Accounts"
fieldName x = x