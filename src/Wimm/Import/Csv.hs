{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Wimm.Import.Csv
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the datatype describing how to import a csv file.

module Wimm.Import.Csv
(
  CsvDescription(..),
  CsvHeader(..),
  CsvRule(..),
  csvRuleCountsAsDefault,
  CsvLineCriterion(..),
  CsvLineResult(..),
  csvAcceptedResult,
  csvResultIsRejected,
  csvResultIsDuplicate,
  csvResultIsMatch,
  csvResultIsDefault,
  importTxns
  )
where

import Data.Maybe (fromMaybe)
import Data.Hashable
import Data.Scientific (Scientific)
import GHC.Generics
import Data.Time (Day, parseTimeM, defaultTimeLocale, iso8601DateFormat)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   Value, Encoding,  Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
import Wimm.Journal
import qualified Data.HashMap.Strict as HM

-- | Defines how to import transactions from a csv bank statement
data CsvDescription = CsvDescription {
  iCsvSeparator :: Char, -- | Csv separator used by the bank
  iAccount1 :: T.Text, -- The account where are importing transactions from
  iAccount2 :: T.Text, -- The default second account
  iSkipHeader :: Bool, -- Should we skip the first line
  iImportNullTxn :: Bool, -- Should we import transactions with 0 amount
  iHeader :: CsvHeader, -- The header, so we can find date, amount, etc.
  iRules :: [CsvRule] -- Rules to apply to each transactions
} deriving (Eq, Show, Generic)

-- The column numbers of the basic info we need
data CsvHeader = CsvHeader {
  csvHeaderDate :: Int,
  csvHeaderAmountIn :: Int,
  csvHeaderAmountOut :: Int,
  cvsHeaderStatementDesc :: Int
} deriving (Eq, Show, Generic)

-- The basic data we extract from a CSV line. The rules will
-- transforms this into a transaction
data CsvLine = CsvLine {
  csvLineAccount1 :: T.Text,
  csvLineDate :: Day,
  csvLineStatementDesc :: T.Text,
  csvLineAmount :: Amount
} deriving (Eq, Show)

-- Rules that can be defined in the JSON/YAML import CSV file
-- They are used to fill in the missing informations to build
-- a transaction
data CsvRule = CsvRule {
  csvRuleCriteria :: [CsvLineCriterion],
  csvRuleAccount2 :: Maybe T.Text, -- Nothing means not to import the transaction
  csvRuleCounterParty :: Maybe T.Text,
  csvRuleTags :: Maybe [T.Text],
  csvRuleComment :: Maybe T.Text,
  -- When reporting, should we consider this rule as a positive match or a default transaction
  csvRuleCountsAsDefaultM :: Maybe Bool 
} deriving (Eq, Show, Generic)

csvRuleCountsAsDefault :: CsvRule -> Bool
csvRuleCountsAsDefault = fromMaybe False . csvRuleCountsAsDefaultM

-- | The results of importing a transaction via a csv
data CsvLineResult = 
    Rejected CsvRule -- A rule specified to drop this transaction
  | Duplicate -- The transaction was removed because it was a duplicate 
  | Accepted (Maybe CsvRule) Transaction -- Nothing means no ruled applied
  deriving (Eq, Show)

csvAcceptedResult :: [CsvLineResult] -> [Transaction]
csvAcceptedResult = foldr go []
  where go (Rejected _) acc = acc
        go Duplicate acc = acc
        go (Accepted _ t) acc = t : acc

csvResultIsRejected :: CsvLineResult -> Bool
csvResultIsRejected (Rejected _) = True
csvResultIsRejected _ = False

csvResultIsDuplicate :: CsvLineResult -> Bool
csvResultIsDuplicate Duplicate = True
csvResultIsDuplicate _ = False

csvResultIsMatch :: CsvLineResult -> Bool
csvResultIsMatch (Accepted (Just x) _) | not (csvRuleCountsAsDefault x) = True
csvResultIsMatch _ = False

csvResultIsDefault :: CsvLineResult -> Bool
csvResultIsDefault (Accepted (Just x) _) | csvRuleCountsAsDefault x = True
csvResultIsDefault (Accepted Nothing _) = True
csvResultIsDefault _ = False

-- How to match a CsvLine
data CsvLineCriterion = MatchStatementDesc T.Text
                      | MatchAmount Amount
                      | AmountAbove Amount
                      | AmountBelow Amount
                      | MatchDate Day
                      deriving (Eq, Show)

matchCriteria :: CsvLine -> CsvLineCriterion -> Bool
matchCriteria l (MatchStatementDesc d) = csvLineStatementDesc l == d
matchCriteria l (MatchAmount m) = csvLineAmount l == m
matchCriteria l (AmountAbove limit) = csvLineAmount l > limit
matchCriteria l (AmountBelow limit) = csvLineAmount l < limit
matchCriteria l (MatchDate d) = csvLineDate l == d

-- Builds the default rules from the JSON import csv file
-- At least one of these rule will always match, so use them
-- after the customs rules
defaultTransaction :: CsvDescription -> CsvLine -> Transaction
defaultTransaction iCsv csvLine = 
  Transaction (csvLineDate csvLine)
              Nothing
              Nothing
              [Posting Nothing (csvLineAccount1 csvLine) (csvLineAmount csvLine),
               Posting Nothing (iAccount2 iCsv) (negate $ csvLineAmount csvLine) ]
              Nothing
              (Just $ csvLineStatementDesc csvLine)

-- The first rule that matches
-- Returns Rejected or Accepted
applyRules :: CsvDescription -> CsvLine -> CsvLineResult
applyRules iCsv csvLine =
  let -- Add the default rules to the user provided rules
      rules = iRules iCsv
      -- The default transaction
      tDefault = defaultTransaction iCsv csvLine
      -- Try to apply the rules
      -- Take the first one that applies
      txn :: CsvLineResult
      txn = foldr matchLine (Accepted Nothing tDefault) rules
  in txn

  where matchLine :: CsvRule -> CsvLineResult -> CsvLineResult
        matchLine r t = if all (matchCriteria csvLine) (csvRuleCriteria r)
                        then mkTxn r
                        else t

        mkTxn :: CsvRule -> CsvLineResult
        mkTxn csvRule = 
          case csvRuleAccount2 csvRule of
            Nothing -> Rejected csvRule
            Just acc2 -> Accepted (Just csvRule) $
              Transaction (csvLineDate csvLine)
                          (csvRuleCounterParty csvRule)
                          (csvRuleTags csvRule)
                          [Posting Nothing (csvLineAccount1 csvLine) (csvLineAmount csvLine),
                           Posting Nothing acc2 (negate $ csvLineAmount csvLine) ]
                          (csvRuleComment csvRule)
                          (Just $ csvLineStatementDesc csvLine)

parseISO8601M :: String -> Either String Day
parseISO8601M s = 
  case parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) s of
    Nothing -> Left $ "failed to parse '" ++ s ++ "' as a date"
    Just x -> return x

readCsvLine :: CsvDescription -> (Int, V.Vector T.Text) -> Either String CsvLine
readCsvLine iCsv (n, line) =
  let acc1 = iAccount1 iCsv
      dateIdx = (csvHeaderDate $ iHeader iCsv) - 1
      amountInIdx = (csvHeaderAmountIn $ iHeader iCsv) - 1
      amountOutIdx = (csvHeaderAmountOut $ iHeader iCsv) - 1
      statementDescIdx =  (cvsHeaderStatementDesc $ iHeader iCsv) - 1
      maxIdx = maximum [dateIdx,amountInIdx,amountOutIdx,statementDescIdx]
      validLength = if V.length line < maxIdx + 1
                    then Left $ "line " ++ show n ++ "does not have at least " ++ show (maxIdx + 1) ++ " columns"
                    else pure ()
  in do
    validLength
    date <- parseISO8601M $ T.unpack $ line V.! dateIdx
    amount <- (-) <$> readAmount (T.unpack $ line V.! amountInIdx)
                  <*> readAmount (T.unpack $ line V.! amountOutIdx)
    let desc = line V.! statementDescIdx
    return (CsvLine acc1 date desc amount)

importTxns :: CsvDescription -> -- The description of the csv file
              V.Vector (V.Vector T.Text) -> -- The content of the file
              Int -> -- The offset of the content for error reporting
              [Transaction] -> -- A list of transactions to test against for duplicates
              Either String [CsvLineResult]
importTxns iCsv myLines lineOffset oldTxns = do
  csvLines <- traverse (readCsvLine iCsv) (V.imap (\i l -> (i + lineOffset,l)) myLines)
  let res = map (applyRules iCsv) (V.toList csvLines)
  return $ removeDuplicateTxns oldTxns res

-- | The part of a transaction that must match for a duplicate detection
data TxnKey = TxnKey {
  tkDate :: Day,
  tkPostings :: [(Identifier, Scientific)],
  tkStatementDesc :: T.Text
} deriving (Eq, Ord, Show, Generic)

instance Hashable TxnKey

toTxnKey :: Transaction -> TxnKey
toTxnKey t = TxnKey (tDate t) 
                    (map (\p -> (pAccount p,toScientific $ pAmount p)) $ tPostings t) 
                    (tStatementDescription t)

-- | removeDuplicateTxns acc old new returns new', the new transactions where
-- the existing one in old are filtered out
removeDuplicateTxns :: [Transaction] -> 
                       [CsvLineResult] -> 
                       [CsvLineResult]
removeDuplicateTxns [] new = new                       
removeDuplicateTxns old new =
  let oldKeys :: HM.HashMap TxnKey Int
      oldKeys = HM.fromListWith (+) 
              $ zip (map toTxnKey old) (repeat 1)
      dedup :: CsvLineResult -> (HM.HashMap TxnKey Int, [CsvLineResult]) -> (HM.HashMap TxnKey Int, [CsvLineResult])
      dedup x@(Rejected _) (m,xs) = (m, x : xs) -- Nothing to filter
      dedup Duplicate (m,xs) = (m, Duplicate : xs) -- Nothing to filter
      dedup x@(Accepted _ t) (m, acc) =
        let k = (toTxnKey t)
            foo :: (Maybe Int -> Maybe (Maybe Int))
            foo Nothing = Nothing -- Simply not in the map
            foo (Just 1) = (Just Nothing) -- Remove the key
            foo (Just n) = (Just (Just (n - 1))) -- Decrement the value
        in case HM.alterF foo k m of
              Nothing -> (m, x : acc) -- Not in old
              Just m' -> (m', acc) -- In old

  in snd $ foldr dedup (oldKeys, []) new

-- ToJSON and FromJason instances
instance ToJSON CsvDescription where
  toJSON = genericToJSON customOptionsCsvDescription
  toEncoding = genericToEncoding customOptionsCsvDescription

instance FromJSON CsvDescription where
  parseJSON = genericParseJSON customOptionsCsvDescription

customOptionsCsvDescription :: Options
customOptionsCsvDescription = defaultOptions{
  fieldLabelModifier = fieldNameCsvDescription,
  omitNothingFields = True
}

fieldNameCsvDescription :: String -> String
fieldNameCsvDescription "iCsvSeparator" = "csv separator"
fieldNameCsvDescription "iAccount1" = "account 1"
fieldNameCsvDescription "iAccount2" = "account 2"
fieldNameCsvDescription "iSkipHeader" = "skip header"
fieldNameCsvDescription "iImportNullTxn" = "import transaction when amount is zero"
fieldNameCsvDescription "iHeader" = "header"
fieldNameCsvDescription "iRules" = "rules"
fieldNameCsvDescription x = x


instance ToJSON CsvHeader where
  toJSON = genericToJSON customOptionsCsvHeader
  toEncoding = genericToEncoding customOptionsCsvHeader

instance FromJSON CsvHeader where
  parseJSON = genericParseJSON customOptionsCsvHeader

customOptionsCsvHeader :: Options
customOptionsCsvHeader = defaultOptions{
  fieldLabelModifier = fieldNameCsvHeader,
  omitNothingFields = True
}

fieldNameCsvHeader :: String -> String
fieldNameCsvHeader "csvHeaderDate" = "date"
fieldNameCsvHeader "csvHeaderAmountIn" = "amount in"
fieldNameCsvHeader "csvHeaderAmountOut" = "amount out"
fieldNameCsvHeader "cvsHeaderStatementDesc" = "statement description"
fieldNameCsvHeader x = x


instance ToJSON CsvRule where
  toJSON = genericToJSON customOptionsCsvRule
  toEncoding = genericToEncoding customOptionsCsvRule

instance FromJSON CsvRule where
  parseJSON = genericParseJSON customOptionsCsvRule

customOptionsCsvRule :: Options
customOptionsCsvRule = defaultOptions{
  fieldLabelModifier = fieldNameCsvRule,
  omitNothingFields = True
}

fieldNameCsvRule :: String -> String
fieldNameCsvRule "csvRuleCriteria" = "criteria"
fieldNameCsvRule "csvRuleAccount2" = "account 2"
fieldNameCsvRule "csvRuleCounterParty" = "counterparty"
fieldNameCsvRule "csvRuleTags" = "tags"
fieldNameCsvRule "csvRuleComment" = "comment"
fieldNameCsvRule "csvRuleCountsAsDefaultM" = "default rule"
fieldNameCsvRule x = x

instance ToJSON CsvLineCriterion where
  toJSON (MatchStatementDesc t) = mkCriterion "statement description" t
  toJSON (MatchAmount amnt) = mkCriterion "amount" (toScientific amnt)
  toJSON (AmountAbove limit) = mkCriterion "amount above" (toScientific limit)
  toJSON (AmountBelow limit) = mkCriterion "amount below" (toScientific limit)
  toJSON (MatchDate d) = mkCriterion "date" d

  toEncoding (MatchStatementDesc t) = mkCriterion2 "statement description" t
  toEncoding (MatchAmount amnt) = mkCriterion2 "amount" (toScientific amnt)
  toEncoding (AmountAbove limit) = mkCriterion2 "amount above" (toScientific limit)
  toEncoding (AmountBelow limit) = mkCriterion2 "amount below" (toScientific limit)
  toEncoding (MatchDate d) = mkCriterion2 "date" d

mkCriterion :: (ToJSON a) => String -> a -> Value
mkCriterion s a = object ["criterion" .= s, "value" .= a]

mkCriterion2 :: (ToJSON a) => String -> a -> Encoding
mkCriterion2 s a = pairs ("criterion" .= s <> "value" .= a)

instance FromJSON CsvLineCriterion where
    parseJSON = withObject "csv line criterion" $ \v -> do
      c <- v .: "criterion"
      case (c :: String) of
        "statement description" -> MatchStatementDesc <$> v .: "value"
        "amount" -> MatchAmount <$> fmap fromScientific (v .: "value")
        "amount above" -> AmountAbove <$> fmap fromScientific (v .: "value")
        "amount below" -> AmountBelow <$> fmap fromScientific (v .: "value")
        "date" -> MatchDate <$> v .: "value"
        _ -> fail ("Invalid criterion '" ++ c ++ "'")