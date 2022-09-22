-- |
-- Module      :  Wimm.Journal.Currency
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what is a currency. Althought wimm is a single currency
-- programm, we need some info about this currency. 

module Wimm.Journal.Currency
    ( Currency(..)
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs)
import qualified Data.Text as T

-- | The description of a currency. 
data Currency = Currency {
    cName :: T.Text, -- Name of the currency.
    cDecimalSep :: T.Text, -- The symbol for the currency decimal separator.
    cNbOfDecimal :: Int, -- How many decimal place for this currency.
    cSymbol :: T.Text, -- The symbol of the currency.
    cSymbolBeforeAmount :: Bool -- Where to place the symbol, before or after the amount.
    }
    deriving (Eq, Show)

instance ToJSON Currency where
  toJSON (Currency name sep nbDec sym symBefore) =
        object ["Name" .= name, 
                "Decimal separator" .= sep,
                "Nb of decimal" .= nbDec,
                "Symbol" .= sym,
                "Symbol before amount" .= symBefore]
  toEncoding (Currency name sep nbDec sym symBefore) =
        pairs  ("Name" .= name <>
                "Decimal separator" .= sep <>
                "Nb of decimal" .= nbDec <>
                "Symbol" .= sym <>
                "Symbol before amount" .= symBefore)
              
instance FromJSON Currency where
    parseJSON = withObject "Currency" $ \v -> Currency
        <$> v .: "Name"
        <*> v .: "Decimal separator"
        <*> v .: "Nb of decimal"
        <*> v .: "Symbol"
        <*> v .: "Symbol before amount"
