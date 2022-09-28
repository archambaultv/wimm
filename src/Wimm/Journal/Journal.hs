{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Wimm.Journal.Journal
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what is a journal, the file read by wimm. 

module Wimm.Journal.Journal
    ( Journal(..),
      jBalanceAssertions,
      jBudgets,
      jCompanyName,
      jReportParams,
      jFirstFiscalMonth,
      jDefaultBudget,
      budgetAccounts,
      accountForest,
      AccountInfo(..),
      accInfoMap,
      accInfoList,
      getDefaultBudget
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Functor.Foldable (cata, para)
import GHC.Generics
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
import Wimm.Journal.Account
import Wimm.Journal.ReportParameters
import Wimm.Journal.Transaction
import Wimm.Journal.Budget
import Wimm.Journal.BalanceAssertion

-- | The Journal is a file that contains all the financial data (transactions)
-- and other info like account descriptions needed to process the data
data Journal = Journal {
  -- | The account in the balance sheet that we must use as the opening balance account.
  jOpeningBalanceAccount :: T.Text,
  
  -- | The account in the balance sheet that we must use as the earnings account.
  jEarningsAccount :: T.Text,
  
  -- | The name of the company or the name to display in the reports
  jCompanyNameM :: Maybe T.Text,

   -- | First month of the fiscal year
  jFirstFiscalMonthM :: Maybe Int,

  -- | The defaults parameters for reporting
  jReportParamsM :: Maybe JournalReportParameters,

  -- | The accounts. One tree per account type
  jAsset :: Account,
  jLiability :: Account,
  jEquity :: Account,
  jRevenue :: Account,
  jExpense :: Account,

  -- | The transactions.
  jTransactions :: [Transaction],

  -- | The balance assertions.
  jBalanceAssertionsM :: Maybe [BalanceAssertion],

  -- | The default budget
  jDefaultBudgetM :: Maybe T.Text,

  -- | The budgets
  jBudgetsM :: Maybe [Budget]
  }
  deriving (Eq, Show, Generic)

jDefaultBudget :: Journal -> T.Text
jDefaultBudget j = 
  case jBudgets j of
    [] -> fromMaybe "" (jDefaultBudgetM j)
    (x:_) -> fromMaybe (bName x) (jDefaultBudgetM j)

-- | Returns the budget defined by default budget
-- The budget must exists or else error will be called
getDefaultBudget :: Journal -> Budget
getDefaultBudget j =
  let budgets = filter (\b -> bName b == (jDefaultBudget j)) (jBudgets j)
  in if null budgets
    then error "getDefaultBudget invalide default budget name"
    else head budgets

jCompanyName :: Journal -> T.Text
jCompanyName j = fromMaybe "" (jCompanyNameM j)

jFirstFiscalMonth :: Journal -> Int
jFirstFiscalMonth j = fromMaybe 1 (jFirstFiscalMonthM j)

jBudgets :: Journal -> [Budget]
jBudgets j = fromMaybe [] (jBudgetsM j)

jReportParams :: Journal -> JournalReportParameters
jReportParams j = fromMaybe (JournalReportParameters '.' ',') (jReportParamsM j)

jBalanceAssertions :: Journal -> [BalanceAssertion]
jBalanceAssertions j = fromMaybe [] (jBalanceAssertionsM j)

instance ToJSON Journal where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Journal where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions{
  fieldLabelModifier = fieldName,
  omitNothingFields = True
}

fieldName :: String -> String
fieldName "jOpeningBalanceAccount" = "opening balance account"
fieldName "jEarningsAccount" = "earnings account"
fieldName "jCompanyNameM" = "company name"
fieldName "jFirstFiscalMonthM" = "first fiscal month"
fieldName "jReportParamsM" = "csv report parameters"
fieldName "jAsset" = "assets account tree"
fieldName "jLiability" = "liabilities account tree"
fieldName "jEquity" = "equity account tree"
fieldName "jRevenue" = "revenue account tree"
fieldName "jExpense" = "expenses account tree"
fieldName "jTransactions" = "transactions"
fieldName "jDefaultBudgetM" = "default budget"
fieldName "jBudgetsM" = "budgets"
fieldName "jBalanceAssertionsM" = "balance assertions"
fieldName x = x

-- | The five top accounts
accountForest :: Journal -> [Account]
accountForest j = [jAsset j, jLiability j, jEquity j, jRevenue j, jExpense j]

-- | Returns the list of all budget accounts (and their children)
-- Does not return an account twice if the parent is also in the list of budget accounts
budgetAccounts :: Journal -> Budget -> [Account]
budgetAccounts j budget = concatMap (para alg)
                        $ accountForest j
  
  where alg :: AccountF (Account, [Account]) -> [Account]
        alg (AccountF ident name number xs) =
          if ident `elem` bAccs
          then [Account ident name number (map fst xs)]
          else concatMap snd xs

        bAccs :: [Identifier]
        bAccs = bAccounts budget

-- | The information associated with an identifier in the journal file
data AccountInfo = AccountInfo {
  aiIdentifier :: T.Text,
  aiAccountType :: AccountType,
  aiNumber :: Int,
  aiDisplayName :: T.Text
}
-- | Returns a map from Identifier to account type
accInfoMap :: Journal -> HM.HashMap Identifier AccountInfo
accInfoMap j = HM.fromList (accInfoList j)

-- | Returns a map from Identifier to account type
accInfoList :: Journal -> [(Identifier, AccountInfo)]
accInfoList j = concatMap (\(accType, f) -> cata (alg accType) (f j))
              [(Asset, jAsset), 
              (Liability, jLiability),
              (Equity, jEquity),
              (Revenue, jRevenue),
              (Expense, jExpense)]
    where alg :: AccountType -> AccountF [(Identifier, AccountInfo)] -> [(Identifier, AccountInfo)]
          alg accType acc@(AccountF ident _ number xs) = 
            (ident, AccountInfo ident accType number (aDisplayNameF acc))
                : concat xs