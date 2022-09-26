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

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, pairs,
                   (.:?),(.!=))

data JournalReportParameters = JournalReportParameters {
  -- | The decimal separator to use when writing amounts in the reports
  jDecimalSep :: Char,

  -- | Csv separator to use when writing reports
  jCsvSeparator :: Char
} deriving (Eq, Show)

instance ToJSON JournalReportParameters where
  toJSON (JournalReportParameters currSep csvSep) =
        object ["Decimal separator" .= currSep,
                "Csv delimiter" .= csvSep]
                
  toEncoding (JournalReportParameters currSep csvSep) =
        pairs $ "Decimal separator" .= currSep <>
                "Csv delimiter" .= csvSep

instance FromJSON JournalReportParameters where
    parseJSON = withObject "Report parameters" $ \v -> JournalReportParameters
        <$> v .:? "Decimal separator" .!= '.'
        <*> v .:? "Csv delimiter" .!= ','