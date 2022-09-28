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

import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Time (Day)
import Data.List (sortOn, groupBy)
import Data.Foldable (traverse_)
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as HM

-- | Checks if the integrety of the journal file
-- Checks for :
--   - Each transaction must balance
--   - Balance assertion correctness
journalCheck :: Journal -> Either String ()
journalCheck j = do
  checkTransactionsBalance j
  checkBalanceAssertion j

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
                  $ map (first tDate) -- [(Day, Posting)]
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