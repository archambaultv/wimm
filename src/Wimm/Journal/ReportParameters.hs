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
    ( JournalReportParameters(..),
      jCsvAddBom
    ) where

import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)

data JournalReportParameters = JournalReportParameters {
  -- | The decimal separator to use when writing amounts in the reports
  jDecimalSep :: Char,

  -- | Csv separator to use when writing csv reports
  jCsvSeparator :: Char,

  -- | Whether to add a UTF-8 BOM at the beginning of the CSV file (ex : Microsoft Excel)
  jCsvAddBomM :: Maybe Bool

} deriving (Eq, Show, Generic)

jCsvAddBom :: JournalReportParameters -> Bool
jCsvAddBom (JournalReportParameters _ _ Nothing) = False
jCsvAddBom (JournalReportParameters _ _ (Just x)) = x

instance ToJSON JournalReportParameters where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON JournalReportParameters where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions{
  fieldLabelModifier = fieldName,
  omitNothingFields = True
}

fieldName :: String -> String
fieldName "jDecimalSep" = "decimal separator"
fieldName "jCsvSeparator" = "csv column separator"
fieldName "jCsvAddBomM" = "csv UTF-8 BOM"
fieldName x = x