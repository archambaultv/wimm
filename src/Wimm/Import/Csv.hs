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
  ImportCsv(..),
  CsvHeader(..),
  CsvRule(..),
  CsvLineCriterion(..),
  importCsv
  )
where

import Data.Char (ord)
import Data.Maybe (catMaybes)
import Data.Time (Day, parseTimeM, defaultTimeLocale, iso8601DateFormat)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   (.:?), (.!=), Value, Encoding)
import Wimm.Journal

-- | Defines how to import transactions from a csv bank statement
data ImportCsv = ImportCsv {
  iCsvSeparator :: Char, -- | Csv separator used by the bank
  iAccount1 :: T.Text, -- The account where are importing transactions from
  iAccount2Negative :: T.Text, -- The default second account when the amount is negative
  iAccount2Positive :: T.Text, -- The default second account when the amount is positive
  iSkipHeader :: Bool, -- Should we skip the first line
  iImportNullTxn :: Bool, -- Should we import transactions with 0 amount
  iHeader :: CsvHeader, -- The header, so we can find date, amount, etc.
  iRules :: [CsvRule] -- Rules to apply to each transactions
} deriving (Eq, Show)

-- The column numbers of the basic info we need
data CsvHeader = CsvHeader {
  csvHeaderDate :: Int,
  csvHeaderAmountIn :: Int,
  csvHeaderAmountOut :: Int,
  cvsHeaderStatementDesc :: Int
} deriving (Eq, Show)

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
  csvRuleAccount2 :: T.Text,
  csvRuleCounterParty :: T.Text,
  csvRuleTags :: [T.Text],
  csvRuleComment :: T.Text
} deriving (Eq, Show)

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
defaultRules :: ImportCsv -> [CsvRule]
defaultRules iCsv = [
  CsvRule [AmountBelow 0] (iAccount2Negative iCsv) "" [] "",
  CsvRule [AmountAbove 0] (iAccount2Positive iCsv) "" [] "",
  CsvRule [MatchAmount 0] (iAccount2Negative iCsv) "" [] ""
  ]

-- The first rule that matches
applyRules :: ImportCsv -> CsvLine -> Transaction
applyRules iCsv csvLine =
  let -- Add the default rules to the user provided rules
      rules = iRules iCsv ++ defaultRules iCsv
      -- Try to apply the rules
      txns :: [Maybe Transaction]
      txns = map matchLine rules
      -- Pick the first one that applies. We know one must apply
      t = head $ catMaybes txns
  in t

  where matchLine :: CsvRule -> Maybe Transaction
        matchLine r = if all (matchCriteria csvLine) (csvRuleCriteria r)
                           then Just (mkTxn r)
                           else Nothing


        mkTxn :: CsvRule -> Transaction
        mkTxn csvRule = 
              Transaction (csvLineDate csvLine)
                          (csvRuleCounterParty csvRule)
                          (csvRuleTags csvRule)
                          [Posting Nothing (csvLineAccount1 csvLine) (csvLineAmount csvLine),
                           Posting Nothing (csvRuleAccount2 csvRule) (negate $ csvLineAmount csvLine) ]
                          (csvRuleComment csvRule)
                          (csvLineStatementDesc csvLine)

parseISO8601M :: String -> Maybe Day
parseISO8601M s = parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) s

readCsvLine :: ImportCsv -> V.Vector T.Text -> Maybe CsvLine
readCsvLine iCsv line =
  let acc1 = iAccount1 iCsv
      dateIdx = (csvHeaderDate $ iHeader iCsv) - 1
      amountInIdx = (csvHeaderAmountIn $ iHeader iCsv) - 1
      amountOutIdx = (csvHeaderAmountOut $ iHeader iCsv) - 1
      statementDescIdx =  (cvsHeaderStatementDesc $ iHeader iCsv) - 1
      date = parseISO8601M $ T.unpack $ line V.! dateIdx
      amount = (-) <$> readAmount (T.unpack $ line V.! amountInIdx)
                   <*> readAmount (T.unpack $ line V.! amountOutIdx)
      desc = line V.! statementDescIdx
  in ((,) <$> date <*> amount)
     >>= \(d, m) -> return (CsvLine acc1 d desc m)

importCsv :: ImportCsv -> FilePath -> IO [Transaction]
importCsv iCsv path = do
  csv <- BL.readFile path :: IO BL.ByteString
  let h = if iSkipHeader iCsv then Csv.HasHeader else Csv.NoHeader
  let opt = Csv.defaultDecodeOptions {
      Csv.decDelimiter = fromIntegral (ord (iCsvSeparator iCsv))
    }
  case (Csv.decodeWith opt h csv :: Either String (V.Vector (V.Vector T.Text))) of
    Left err -> putStrLn err >> return []
    Right myLines -> 
      case traverse (readCsvLine iCsv) myLines of
        Nothing -> putStrLn "Unable to parse a line" >> return []
        Just csvLines -> return $ map (applyRules iCsv) (V.toList csvLines)

-- ToJSON and FromJason instances
instance ToJSON ImportCsv where
  toJSON (ImportCsv csvSep acc1 acc2N acc2P skipHeader importNull header rules) =
        object $ ["Csv separator" .= csvSep,
                  "Account 1" .= acc1,
                  "Account 2 if amount is negative" .= acc2N,
                  "Account 2 if amount is positive" .= acc2P,
                  "Skip header" .= skipHeader,
                  "Import transaction when amount is zero" .= importNull,
                  "Header" .= header] ++
                (if null rules then [] else ["Rules" .= rules])
  toEncoding (ImportCsv csvSep acc1 acc2N acc2P skipHeader importNull header rules) =
        pairs $ "Csv separator" .= csvSep <>
                  "Account 1" .= acc1 <>
                  "Account 2 if amount is negative" .= acc2N <>
                  "Account 2 if amount is positive" .= acc2P <>
                  "Skip header" .= skipHeader <>
                  "Import transaction when amount is zero" .= importNull <>
                  "Header" .= header <>
                (if null rules then mempty else "Rules" .= rules)

instance FromJSON ImportCsv where
    parseJSON = withObject "ImportCsv" $ \v -> ImportCsv
        <$> v .:? "Csv separator" .!= ','
        <*> v .: "Account 1"
        <*> v .: "Account 2 if amount is negative"
        <*> v .: "Account 2 if amount is positive"
        <*> v .: "Skip header"
        <*> v .:? "Import transaction when amount is zero" .!= True
        <*> v .: "Header"
        <*> v .:? "Rules" .!= []


instance ToJSON CsvHeader where
  toJSON (CsvHeader date amountIn amountOut desc) =
        object ["Date" .= date,
                  "Amount in" .= amountIn,
                  "Amount out" .= amountOut,
                  "Statement description" .= desc]
  toEncoding (CsvHeader date amountIn amountOut desc) =
        pairs ("Date" .= date <>
              "Amount in" .= amountIn <>
              "Amount out" .= amountOut <>
              "Statement description" .= desc)

instance FromJSON CsvHeader where
    parseJSON = withObject "CsvHeader" $ \v -> CsvHeader
        <$> v .: "Date"
        <*> v .: "Amount in"
        <*> v .: "Amount out"
        <*> v .: "Statement description"


instance ToJSON CsvRule where
  toJSON (CsvRule criteria acc2 ccp tags comment) =
        object $ ["Criteria" .= criteria,
                  "Account 2" .= acc2] ++
                (if T.null ccp then [] else ["Counterparty" .= ccp]) ++
                (if null tags then [] else ["Tags" .= tags]) ++
                (if T.null comment then [] else ["Comment" .= comment])

  toEncoding (CsvRule criteria acc2 ccp tags comment) =
        pairs $ "Criteria" .= criteria <>
                "Account 2" .= acc2 <>
                (if T.null ccp then mempty else "Counterparty" .= ccp) <>
                (if null tags then mempty else "Tags" .= tags) <>
                (if T.null comment then mempty else "Comment" .= comment)

instance FromJSON CsvRule where
    parseJSON = withObject "CsvRule" $ \v -> CsvRule
        <$> v .: "Criteria"
        <*> v .: "Account 2"
        <*> v .:? "Counterparty" .!= ""
        <*> v .:? "Tags" .!= []
        <*> v .:? "Comment" .!= ""

instance ToJSON CsvLineCriterion where
  toJSON (MatchStatementDesc t) = mkCriterion "Statement description" t
  toJSON (MatchAmount amnt) = mkCriterion "Amount" (toScientific amnt)
  toJSON (AmountAbove limit) = mkCriterion "Amount above" (toScientific limit)
  toJSON (AmountBelow limit) = mkCriterion "Amount below" (toScientific limit)
  toJSON (MatchDate d) = mkCriterion "Date" d

  toEncoding (MatchStatementDesc t) = mkCriterion2 "Statement description" t
  toEncoding (MatchAmount amnt) = mkCriterion2 "Amount" (toScientific amnt)
  toEncoding (AmountAbove limit) = mkCriterion2 "Amount above" (toScientific limit)
  toEncoding (AmountBelow limit) = mkCriterion2 "Amount below" (toScientific limit)
  toEncoding (MatchDate d) = mkCriterion2 "Date" d

mkCriterion :: (ToJSON a) => String -> a -> Value
mkCriterion s a = object ["Criterion" .= s, "Value" .= a]

mkCriterion2 :: (ToJSON a) => String -> a -> Encoding
mkCriterion2 s a = pairs ("Criterion" .= s <> "Value" .= a)

instance FromJSON CsvLineCriterion where
    parseJSON = withObject "CsvLineCriterion" $ \v -> do
      c <- v .: "Criterion"
      case (c :: String) of
        "Statement description" -> MatchStatementDesc <$> v .: "Value"
        "Amount" -> MatchAmount <$> fmap fromScientific (v .: "Value")
        "Amount above" -> AmountAbove <$> fmap fromScientific (v .: "Value")
        "Amount below" -> AmountBelow <$> fmap fromScientific (v .: "Value")
        "Date" -> MatchDate <$> v .: "Value"
        _ -> fail ("Invalid criterion '" ++ c ++ "'")