{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Wimm.Journal.ReportParameters
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the report parameters that can be defined in the journal file

module Wimm.Journal.ReportParameters
    ( JournalReportParameters(..)
    ) where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)

data JournalReportParameters = JournalReportParameters {
  -- | The decimal separator to use when writing amounts in the reports
  jDecimalSep :: Char,

  -- | Csv separator to use when writing reports
  jCsvSeparator :: Char
} deriving (Eq, Show, Generic)

instance ToJSON JournalReportParameters where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON JournalReportParameters where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions{
  fieldLabelModifier = fieldName
}

fieldName :: String -> String
fieldName "jDecimalSep" = "decimal separator"
fieldName "jCsvSeparator" = "csv column separator"
fieldName x = x