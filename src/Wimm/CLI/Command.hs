-- |
-- Module      :  Wimm.CLI.Commands
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines the commands available when using wimm

module Wimm.CLI.Command
(
  Command(..),
  ImportCmd(..),
  runCommand
) where

import Control.Monad.Except (runExceptT, ExceptT(..), lift, liftEither)
import qualified Data.Yaml as Yaml
import qualified Data.Csv as Csv
import qualified Data.Yaml.Pretty as YamlP
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSONP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Char (ord)
import Data.List (sortOn, intercalate)
import Wimm.Journal
import Wimm.Import.Csv
import Wimm.Report
import Data.Char (toLower)
import System.FilePath (takeExtension)

-- | The commands accepted by the command line interface
data Command = CTxnReport FilePath FilePath TxnReportParams
             | CBalSheetReport FilePath FilePath
             | CBudgetReport FilePath FilePath
             | CIncomeStatementReport FilePath FilePath
             | CTxnImport ImportCmd
             | CCheck FilePath

data ImportCmd = ImportCmd {
  icCsvDescPath :: FilePath,
  icCsvDataPath :: FilePath,
  icOutputPath :: FilePath,
  icJournalPath :: Maybe FilePath,
  icNbOfDefaultTxnToDisplay :: Int
}

-- | How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand c = runExceptT (runCommand' c) >>= either putStrLn return

runCommand' :: Command -> ExceptT String IO ()
runCommand' (CTxnReport journalPath reportPath params) = 
  runReport journalPath reportPath (transactionReport params)

runCommand' (CBalSheetReport journalPath reportPath) =
  runReport journalPath reportPath (balanceSheetReport (Nothing, Nothing))

runCommand' (CBudgetReport journalPath reportPath) =
  let myReport j = budgetReport (Nothing, Nothing) j (getDefaultBudget j)
  in runReport journalPath reportPath myReport

runCommand' (CIncomeStatementReport journalPath reportPath) =
  runReport journalPath reportPath (incomeStatementReport (Nothing, Nothing))

runCommand' (CTxnImport cmd) = do
  -- Decode the csv description and the csv file itself
  desc <- decodeFileByExt (icCsvDescPath cmd)
  csv <- lift $ BL.readFile (icCsvDataPath cmd)
  let (h,lineOffset) = if iSkipHeader desc then (Csv.HasHeader, 1) else (Csv.NoHeader, 0)
  let opt = Csv.defaultDecodeOptions {
      Csv.decDelimiter = fromIntegral (ord (iCsvSeparator desc))
    }
  csvLines <- liftEither (Csv.decodeWith opt h csv :: Either String (V.Vector (V.Vector T.Text)))
  
  -- Verify if we have to filter with existing transactions
  myfilter <- case (icJournalPath cmd) of
              Nothing -> pure Nothing
              Just jPath -> fmap Just $ decodeJournal jPath

  -- Extract the transactions from the csv file
  lineResults <- liftEither $ importTxns desc csvLines lineOffset myfilter
  txns <- lift $ printImportReport lineResults (icNbOfDefaultTxnToDisplay cmd)

  -- Build report and write transactions to file
  encodeFileByExt (icOutputPath cmd) configTxnJSON configTxnYaml txns

runCommand' (CCheck journalPath) = do
  _ <- decodeJournal journalPath -- Decode journal calls journalCheck
  lift $ putStrLn "Journal file OK"

printImportReport :: [CsvLineResult] -> Int -> IO [Transaction]
printImportReport xs topDefaultNb =
  let nbOfLines = show $ length xs
      defaults :: [Transaction]
      defaults = csvDefaultResult xs
      nbOfRejected = show $ length $ filter csvResultIsRejected xs
      nbOfDuplicates = show $ length $ filter csvResultIsDuplicate xs
      nbOfMatch = show $ length $ filter csvResultIsMatch xs
      nbOfDefault = show $ length $ defaults
      widthN = maximum $ map length [nbOfLines, nbOfRejected, nbOfDuplicates, nbOfMatch, nbOfDefault]
      pad :: Int -> String -> String
      pad width s = replicate (width - length s) ' ' ++ s
      topDefaultNb' = max 0 topDefaultNb
      topDefault = take topDefaultNb'
                 $ reverse
                 $ sortOn snd
                 $ HM.toList
                 $ HM.fromListWith (+)
                 $ map (\t -> (tStatementDescription t, (1 :: Int))) defaults
      widthDefN = maximum $ map (length . show . snd) topDefault
      widthDefS = maximum $ map (T.length . fst) topDefault
      rpad :: Int -> String -> String
      rpad width s = s ++ replicate (width - length s) ' '
      topDefaultStr = intercalate "\n"
                    $ map (\(d,n) ->rpad widthDefS (T.unpack d) ++ " : " ++ pad widthDefN (show n))
                    $ topDefault
  in do
    putStrLn $ "Number of csv lines       : " ++ pad widthN nbOfLines
    putStrLn $ "Number of rejected lines  : " ++ pad widthN nbOfRejected
    putStrLn $ "Number of duplicate lines : " ++ pad widthN nbOfDuplicates
    putStrLn $ "Number of matched lines   : " ++ pad widthN nbOfMatch
    putStrLn $ "Number of default lines   : " ++ pad widthN nbOfDefault
    (if null defaults || topDefaultNb' == 0
     then return () 
     else do
      putStrLn ""
      putStrLn "Statement descriptions with the most default transactions"
      putStrLn topDefaultStr)

    return $ csvAcceptedResult xs

runReport :: FilePath -> FilePath -> (Journal -> Report) -> ExceptT String IO ()
runReport journalPath reportPath mkReport = do
  journal <- decodeJournal journalPath
  let report = mkReport journal
  let csvSep = jCsvSeparator $ jReportParams journal
  let reportBL = writeReport csvSep report
  let maybeBom = if jCsvAddBom $ jReportParams journal then bom else ""
  lift $ BL.writeFile reportPath $ BL.concat [maybeBom, reportBL]

bom :: BL.ByteString
bom = BL.pack [0xEF,0xBB,0xBF]

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeJournal :: FilePath -> ExceptT String IO Journal
decodeJournal path = do
  j <- ExceptT $ case map toLower (takeExtension path) of
                ".json" -> JSON.eitherDecodeFileStrict path :: IO (Either String Journal)
                _ -> fmap (either (Left . show) return) 
                    (Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Journal))
  liftEither $ journalCheck j
  return j

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
encodeFileByExt :: (JSON.ToJSON a) => FilePath -> JSONP.Config -> YamlP.Config -> a -> ExceptT String IO ()
encodeFileByExt path jsonConfig yamlConfig x = lift $
  case map toLower (takeExtension path) of
    ".json" -> BL.writeFile path $ JSONP.encodePretty' jsonConfig x
    _ -> BS.writeFile path $ YamlP.encodePretty yamlConfig x

-- Decodes with pure JSON for .json file. Any other extension is decoded with in
-- YAML
decodeFileByExt :: (JSON.FromJSON a) => FilePath -> ExceptT String IO a
decodeFileByExt path  = ExceptT $ 
  case map toLower (takeExtension path) of
    ".json" -> JSON.eitherDecodeFileStrict' path
    _ -> fmap (either (Left . show) return) $ Yaml.decodeFileEither path

txnConfCompare :: T.Text -> T.Text -> Ordering
txnConfCompare = JSONP.keyOrder ["date","id","postings","comment","counterparty","tags","statement description"]
                      `mappend` compare 

configTxnYaml :: YamlP.Config
configTxnYaml = YamlP.setConfCompare txnConfCompare YamlP.defConfig

configTxnJSON :: JSONP.Config
configTxnJSON = JSONP.defConfig { 
  JSONP.confIndent = JSONP.Spaces 2, 
  JSONP.confCompare = txnConfCompare
  }