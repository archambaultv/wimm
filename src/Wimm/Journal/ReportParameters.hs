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

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   (.:?),(.!=))
import qualified Data.Text as T

data JournalReportParameters = JournalReportParameters {
  -- | The account in the balance sheet that we must use as the opening balance account.
  jOpeningBalanceAccount :: T.Text,
  
  -- | The account in the balance sheet that we must use as the earnings account.
  jEarningsAccount :: T.Text,
  
  -- | The name of the company or the name to display in the reports
  jCompanyName :: T.Text,

   -- | First month of the fiscal year
  jFirstFiscalMonth :: Int,

  -- | The decimal separator to use when writing amounts in the reports
  jDecimalSep :: Char,

  -- | Csv separator to use when writing reports
  jCsvSeparator :: Char
} deriving (Eq, Show)

instance ToJSON JournalReportParameters where
  toJSON (JournalReportParameters open earn comp ffm currSep csvSep) =
        object $ (if T.null comp then [] else ["Company name" .= comp]) ++
               ["Opening balance account" .= open, 
                "Earnings account" .= earn,
                "First fiscal month" .= ffm,
                "Decimal separator" .= currSep,
                "CSV delimiter" .= csvSep]
                
  toEncoding (JournalReportParameters open earn comp ffm currSep csvSep) =
        pairs $ (if T.null comp then mempty else "Company name" .= comp) <>
                "Opening balance account" .= open <>
                "Earnings account" .= earn <>
                "First fiscal month" .= ffm <>
                "Decimal separator" .= currSep <>
                "CSV delimiter" .= csvSep

instance FromJSON JournalReportParameters where
    parseJSON = withObject "Report parameters" $ \v -> JournalReportParameters
        <$> v .: "Opening balance account"
        <*> v .: "Earnings account"
        <*> v .:? "Company name" .!= ""
        <*> v .:? "First fiscal month" .!= 1
        <*> v .:? "Decimal separator" .!= '.'
        <*> v .:? "CSV delimiter" .!= ','