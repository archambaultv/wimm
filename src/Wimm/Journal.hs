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

import qualified Data.HashSet as HS
import Data.List (intercalate, sort, group)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Time (Day)
import Data.List (sortOn, groupBy)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM

-- | Checks if the integrety of the journal file
-- Checks that :
--   - The account identifiers are unique
--   - The account numbers are unique
--   - The first fiscal month is valid
--   - Declared opening balance account is an actual account
--   - Declared earnings account is an actual account
--   - Each transaction has valid account identifiers
--   - Each transaction has at least 2 postings
--   - Each transaction balances to zero
--   - Each balance assertion has a valid account identifier
--   - Each balance assertion is valid
--   - Default budget is a valid budget name
--   - Each budget has a unique name
--   - Each budget has valid account identifiers
journalCheck :: Journal -> Either String ()
journalCheck j = do
  checkAccounts j
  checkFirstFiscalMonth j
  checkOpeningBalanceAccountExists j
  checkEarningsAccountExists j
  checkTransactions j
  checkBalanceAssertion j
  checkBudget j

checkBudget :: Journal -> Either String ()
checkBudget j = do
  checkBudgetNamesUnique
  checkDefaultBudget
  checkBudgetIdentifiers

  where checkDefaultBudget :: Either String ()
        checkDefaultBudget =
          case jDefaultBudgetM j of
            Nothing -> return () -- Nothing to check
            Just n ->
              if any ((n ==) . bName) (jBudgets j)
              then return ()
              else Left $ "default budget '" ++ T.unpack n ++  "' is not a valid budget name."

        checkBudgetNamesUnique :: Either String ()
        checkBudgetNamesUnique = noDup "budget names" T.unpack (map bName $ jBudgets j)

        checkBudgetIdentifiers :: Either String ()
        checkBudgetIdentifiers = 
          let go :: (Int, Budget) -> Either String ()
              go (n, b) =
                case filter (not . flip HS.member accHS) (bAccounts b) of
                  [] -> return ()
                  bads ->               
                    let badsList :: String
                        badsList = intercalate ", "
                                $ map (\i -> "'" ++ T.unpack i ++ "'") bads
                    in Left $ "budget[" ++ show n ++ "] unknown identifiers : " 
                            ++ badsList

          in traverse_ go (zip [0..] $ jBudgets j)


        accHS :: HS.HashSet Identifier
        accHS = HS.fromList $ map fst $ accInfoList j

        

noDup :: (Ord a) => String -> (a -> String) -> [a] -> Either String ()
noDup name toStr xs = 
  case filter (not . null . tail) (group $ sort xs) of
    [] -> return ()
    bads -> 
      let badsList :: String
          badsList = intercalate ", "
                    $ map (\l -> "'" ++ toStr (head l) ++ "'") bads
      in Left $ "duplicate " ++ name ++ " : " ++ badsList

checkAccounts :: Journal -> Either String ()
checkAccounts j =
  let accs = accInfoList j
      idents = map fst accs
      numbers = map (aiNumber . snd) accs
  in do
    noDup "account identifiers" T.unpack idents
    noDup "account numbers" show numbers

checkFirstFiscalMonth :: Journal -> Either String ()
checkFirstFiscalMonth j = 
  let n = jFirstFiscalMonth j
  in if n < 1 || n > 12
     then Left $ "first fiscal month invalid month number '" ++ show n ++ "'"
     else return ()

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

checkTransactions :: Journal -> Either String ()
checkTransactions j = traverse_ runChecks txns
  where txns :: [(Int, Transaction)]
        txns = zip [0..] (jTransactions j)

        accHS :: HS.HashSet Identifier
        accHS = HS.fromList $ map fst $ accInfoList j

        runChecks :: (Int, Transaction) -> Either String ()
        runChecks t = do
          checkIdent t
          checkPostingsAmount t
          checkBal t

        checkIdent :: (Int, Transaction) -> Either String ()
        checkIdent (n, t) = 
          case filter (not . flip HS.member accHS . pAccount) (tPostings t) of
            [] -> return ()
            bads -> 
              let badsList :: String
                  badsList = intercalate ", "
                           $ map (\p -> "'" ++ T.unpack (pAccount p) ++ "'") bads
              in Left $ "transaction[" ++ show n ++ "] unknown identifiers : " 
                      ++ badsList

        checkPostingsAmount :: (Int, Transaction) -> Either String ()
        checkPostingsAmount (n, t) = 
          let ps = tPostings t
          in if null ps || null (tail ps)
             then Left $ "transaction[" ++ show n ++ "] has less than 2 postings"
             else return ()

        checkBal :: (Int, Transaction) -> Either String ()
        checkBal (n, t) = 
          let txnBalance = sum
                         $ map pAmount
                         $ tPostings t
          in if txnBalance == 0
             then return ()
             else Left  $ "transaction[" ++ show n ++ "] "
                       ++ "does not balance to zero."
                       ++ "\nActual balance " ++ show txnBalance

checkBalanceAssertion :: Journal -> Either String ()
checkBalanceAssertion j = traverse_ runCheck balAssertByIdent
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

      accHS :: HS.HashSet Identifier
      accHS = HS.fromList $ map fst $ accInfoList j

      runCheck :: [(Int, BalanceAssertion)] -> Either String ()
      runCheck t = do
        checkIdent t
        checkBalance t

      -- All the postings related to an account in increasing order of dates
      -- When provided, we use the balance date instead of the transaction date
      postingsMap :: HM.HashMap Identifier [(Day, Posting)]
      postingsMap = HM.fromListWith (++)
                  $ map (\(d, p) -> (pAccount p, [(d,p)])) -- [(Identifier, [(Day, Posting)])]
                  $ reverse
                  $ sortOn fst
                  $ map (\(t, p) -> (fromMaybe (tDate t) (pBalanceDate p), p)) -- [(Day, Posting)]
                  $ concatMap txnToPostings -- [(Transaction, Posting)]
                  $ jTransactions j

      -- check identifier are valid in balance assertion
      -- assuming all balance assertions in the list have the same identifier
      checkIdent :: [(Int, BalanceAssertion)] -> Either String ()
      checkIdent [] = return ()
      checkIdent bs@((_,x):_) =
        if not $ HS.member (baAccount x) accHS
        then let bads = intercalate ", " $ map (show . fst) bs
             in Left $ "balance assertion[" ++ bads 
                      ++ "] unknown identifier : " ++ T.unpack (baAccount x)
        else return ()

      -- Check the validity of a set of balance assertions all related to the
      -- same account. The list must be in increasing order of dates. Simply
      -- sums the postings and for each balance assertion dates verify that the
      -- sum match the assertion.
      checkBalance :: [(Int, BalanceAssertion)] -> Either String ()
      checkBalance bs =
        let ident = baAccount $ snd $ head bs
            
            postings :: [(Day, Posting)]
            postings = fromMaybe [] $ HM.lookup ident postingsMap

            verifySum :: (Int, BalanceAssertion) ->
                         ((Amount, [(Day, Posting)]) -> Either String ()) ->  
                          (Amount, [(Day, Posting)]) -> Either String ()
            -- No more posting, balance assertion must match the current balance amount
            verifySum (n, b) nextBalanceAssertion acc@(balance, []) = do
              match n balance (baAmount b)
              nextBalanceAssertion acc
            -- Still some postings
            verifySum (n, b) nextBalanceAssertion acc@(balance, accps@((d, _):_)) =
              if baDate b < d
              then do 
              -- If the balance assertion date is before the next posting, it
              -- must match the current balance amount. Then we go on to the
              -- next balance without consuming any posting since they might
              -- also occur after the next balance date.
                    match n balance (baAmount b)
                    nextBalanceAssertion acc
              else 
              -- The balance assertion is equal or after the next posting, we
              -- update the balance by consuming all postings until we reach the
              -- next day of the assertion date
                  let (toSum,after) = span ((baDate b >=) . fst) accps
                      newBalance = balance + sum (map (pAmount . snd) toSum)
                  in do
                    match n newBalance (baAmount b)
                    nextBalanceAssertion (newBalance, after)

            match :: Int -> Amount -> Amount -> Either String ()
            match _ x y | x == y = return ()
            match n x y = Left
                        $ "Balance assertion[" ++ show n ++ "] "
                        ++ "does not match the actual balance."
                        ++ "\nActual balance " ++ show x
                        ++ "\nAsserted balance " ++ show y
                        ++ "\nDifference : " ++ show (x - y)
        in foldr verifySum (const $ return ()) bs (0, postings)