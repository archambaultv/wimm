-- |
-- Module      :  Wimm.Journal
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what is a journal, the file read by wimm. 

module Wimm.Journal
    ( Journal(..)
    ) where

import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Wimm.Account
import Wimm.Currency
import Wimm.Transaction

-- | The Journal is a file that contains all the financial data (transactions)
-- and other info like account descriptions needed to process the data
data Journal = Journal {
   -- | The account in the balance sheet that we must use as the opening balance account.
   jOpeningBalanceAccount :: T.Text,
   
   -- | The account in the balance sheet that we must use as the earnings account.
   jEarningsAccount :: T.Text,
   
   -- | The name of the company or the name to display in the reports
   jCompanyName :: T.Text,

    -- | First month of the fiscal year
   jFirstFiscalMonth :: Int,

   -- | Currency description
   jCurrency :: Currency,

   -- | The accounts. For now we use a flat structure
   jAsset :: [Account],
   jLiability :: [Account],
   jEquity :: [Account],
   jRevenue :: [Account],
   jExpense :: [Account],

  -- | The transactions.
  jTransactions :: [Transaction]

  }
  deriving (Eq, Show, Generic)

instance ToJSON Journal where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Journal