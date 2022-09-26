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
  ImportCsv(..),
  CsvHeader(..),
  CsvRule(..),
  CsvLineCriterion(..),
  importCsv
  )
where

import GHC.Generics
import Data.Char (ord)
import Data.Maybe (catMaybes)
import Data.Time (Day, parseTimeM, defaultTimeLocale, iso8601DateFormat)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), pairs,
                   Value, Encoding,  Options(..),toEncoding, genericToEncoding, 
                   genericToJSON, genericParseJSON, defaultOptions)
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
  csvRuleAccount2 :: T.Text,
  csvRuleCounterParty :: Maybe T.Text,
  csvRuleTags :: Maybe [T.Text],
  csvRuleComment :: Maybe T.Text
} deriving (Eq, Show, Generic)

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
  CsvRule [AmountBelow 0] (iAccount2Negative iCsv) Nothing Nothing Nothing,
  CsvRule [AmountAbove 0] (iAccount2Positive iCsv) Nothing Nothing Nothing,
  CsvRule [MatchAmount 0] (iAccount2Negative iCsv) Nothing Nothing Nothing
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
                          (Just $ csvLineStatementDesc csvLine)

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
  toJSON = genericToJSON customOptionsImportCsv
  toEncoding = genericToEncoding customOptionsImportCsv

instance FromJSON ImportCsv where
  parseJSON = genericParseJSON customOptionsImportCsv

customOptionsImportCsv :: Options
customOptionsImportCsv = defaultOptions{
  fieldLabelModifier = fieldNameImportCsv
}

fieldNameImportCsv :: String -> String
fieldNameImportCsv "iAccount1" = "account 1"
fieldNameImportCsv "iAccount2Negative" = "account 2 if amount is negative"
fieldNameImportCsv "iAccount2Positive" = "account 2 if amount is positive"
fieldNameImportCsv "iSkipHeader" = "skip header"
fieldNameImportCsv "import transaction when amount is zero" = "name"
fieldNameImportCsv "iHeader" = "header"
fieldNameImportCsv "iRules" = "rules"
fieldNameImportCsv x = x


instance ToJSON CsvHeader where
  toJSON = genericToJSON customOptionsCsvHeader
  toEncoding = genericToEncoding customOptionsCsvHeader

instance FromJSON CsvHeader where
  parseJSON = genericParseJSON customOptionsCsvHeader

customOptionsCsvHeader :: Options
customOptionsCsvHeader = defaultOptions{
  fieldLabelModifier = fieldNameCsvHeader
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
  fieldLabelModifier = fieldNameCsvRule
}

fieldNameCsvRule :: String -> String
fieldNameCsvRule "csvRuleCriteria" = "criteria"
fieldNameCsvRule "csvRuleAccount2" = "account 2"
fieldNameCsvRule "csvRuleCounterParty" = "counterparty"
fieldNameCsvRule "csvRuleTags" = "tags"
fieldNameCsvRule "csvRuleComment" = "comment"
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