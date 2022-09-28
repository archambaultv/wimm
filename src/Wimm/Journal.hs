-- |
-- Module      :  Wimm.Journal
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module reexports all the data types and functions related to the journal
-- file.

module Wimm.Journal
(
  module Wimm.Journal.Posting,
  module Wimm.Journal.Transaction,
  module Wimm.Journal.Journal,
  module Wimm.Journal.Account,
  module Wimm.Journal.Amount,
  module Wimm.Journal.ReportParameters,
  module Wimm.Journal.Budget,
  module Wimm.Journal.BalanceAssertion,
  journalCheck
  )
where

import Wimm.Journal.Posting
import Wimm.Journal.Transaction
import Wimm.Journal.Journal
import Wimm.Journal.Account
import Wimm.Journal.Amount
import Wimm.Journal.ReportParameters
import Wimm.Journal.Budget
import Wimm.Journal.BalanceAssertion

import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Time (Day)
import Data.List (sortOn, groupBy)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM

-- | Checks if the integrety of the journal file
-- Checks that :
--   - Declared opening balance account is an actual account
--   - Declared earnings account is an actual account
--   - Each transaction must balance
--   - Balance assertions are valid
journalCheck :: Journal -> Either String ()
journalCheck j = do
  checkOpeningBalanceAccountExists j
  checkEarningsAccountExists j
  checkTransactionsBalance j
  checkBalanceAssertion j
  -- TODO
  -- check that first fiscal month is valid
  -- check account identifiers are unique
  -- check account numbers are unique
  -- check identifier are valid in txns
  -- check each transactions have at least 2 postings
  -- check identifier are valid in balance assertion
  -- check identifier are valid in budget
  -- check default budget is a valid budget name

checkOpeningBalanceAccountExists :: Journal -> Either String ()
checkOpeningBalanceAccountExists = checkAccIdent jOpeningBalanceAccount "opening balance account "

checkEarningsAccountExists :: Journal -> Either String ()
checkEarningsAccountExists = checkAccIdent jEarningsAccount "earnings account "

checkAccIdent :: (Journal -> T.Text) -> String -> Journal -> Either String ()
checkAccIdent foo errMsg j =
  let acc = foo j
      identifiers = map fst $ accInfoList j
  in if acc `elem` identifiers
     then return ()
     else Left $ errMsg ++ "'" ++ (T.unpack acc)
               ++ "' is not a valid account identifier."

checkTransactionsBalance :: Journal -> Either String ()
checkTransactionsBalance j = traverse_ checkTxn txns
  where txns :: [(Int, Transaction)]
        txns = zip [0..] (jTransactions j)

        checkTxn :: (Int, Transaction) -> Either String ()
        checkTxn (n, t) = 
          let txnBalance = sum
                         $ map pAmount
                         $ tPostings t
          in if txnBalance == 0
             then return ()
             else Left  $ "transaction[" ++ show n ++ "] "
                       ++ "does not balance to zero."
                       ++ "\nActual balance " ++ show txnBalance

checkBalanceAssertion :: Journal -> Either String ()
checkBalanceAssertion j = traverse_ checkBalance balAssertByIdent
  where
      -- We will proceed one account at a time,
      -- in increasing order of dates. We keep track
      -- of the position of the balance assertion in the journal file
      -- for better error reporting
      balAssertByIdent :: [[(Int, BalanceAssertion)]]
      balAssertByIdent = map (sortOn (baDate . snd))
                       $ groupBy ((==) `on` (baAccount . snd))
                       $ sortOn (baAccount . snd)
                       $ zip [0..] 
                       $ jBalanceAssertions j

      -- All the postings related to an account in increasing order of dates
      postingsMap :: HM.HashMap Identifier [(Day, Posting)]
      postingsMap = HM.fromListWith (++)
                  $ map (\(d, p) -> (pAccount p, [(d,p)])) -- [(Identifier, [(Day, Posting)])]
                  $ reverse
                  $ sortOn fst
                  $ map (\(t, p) -> (fromMaybe (tDate t) (pBalanceDate p), p)) -- [(Day, Posting)]
                  $ concatMap txnToPostings -- [(Transaction, Posting)]
                  $ jTransactions j

      -- Check the validity of a set of balance assertions all related to the
      -- same account. The list must be in increasing order of dates. Simply
      -- sums the postings and for each balance assertion dates verify that the
      -- sum match the assertion.
      checkBalance :: [(Int, BalanceAssertion)] -> Either String ()
      checkBalance [] = return ()
      checkBalance bs =
        let ident = baAccount $ snd $ head bs
            ps = fromMaybe [] $ HM.lookup ident postingsMap
            verifySum :: [(Day, Posting)] -> [(Int, BalanceAssertion)] -> Amount -> Either String ()
            -- No more balance assertion, we are done
            verifySum _ [] _ = return ()
            -- No more posting, it must match the current balance amount
            verifySum [] ((n, BalanceAssertion _ _ amnt) : xs) balance = do
              match n balance amnt
              verifySum [] xs balance
            -- Still some postings and balance assertions
            verifySum xs@((d,p):xss) ys@((n, BalanceAssertion date _ amnt):yss) balance =
              if date < d 
              then do 
              -- If the balance assertion date is before the next posting, it must match the current balance amount
                    match n balance amnt
                    verifySum xs yss balance
              else 
              -- The balance assertion is equal or after the next posting, we update the balance
                  verifySum xss ys (balance + pAmount p)

            match :: Int -> Amount -> Amount -> Either String ()
            match _ x y | x == y = return ()
            match n x y = Left
                        $ "Balance assertion[" ++ show n ++ "] "
                        ++ "does not match the actual balance."
                        ++ "\nActual balance " ++ show x
                        ++ "\nAsserted balance " ++ show y
                        ++ "\nDifference : " ++ show (x - y)
        in verifySum ps bs 0