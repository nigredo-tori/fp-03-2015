{-# LANGUAGE OverloadedStrings, FlexibleInstances, OverlappingInstances #-}

module BugDB where

import           Control.Applicative hiding (empty)
import           Control.Arrow (first)
import           Control.Monad.Trans.Error
import           Data.Attoparsec.Text.Lazy ((<?>), endOfLine, isEndOfLine, many1, takeTill, sepBy, sepBy1, char, endOfInput)
import qualified Data.Attoparsec.Text.Lazy as AP
import qualified Data.ByteString.Lazy as BL
import           Data.HashMap.Strict (HashMap, empty, singleton, fromList, toList, unionWith, keys, elems)
import qualified Data.HashMap.Strict as HS
import           Data.Hashable (Hashable)
import           Data.List (foldl', nub)
import qualified Data.List as L
import           Data.Maybe (isJust, fromJust)
import           Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Prelude hiding (takeWhile, lookup)

-- Data types

type BugID = TL.Text
type CountryID = TL.Text
type Frequency = TL.Text
type BugData = HashMap CountryID Frequency
type BugDB = HashMap BugID BugData

type Coefficient = Int

data BugInfo = BugInfo {
      bugName :: BugID
    , bugData :: BugData
} deriving (Show, Eq)

-- DB management functions

emptyDb :: BugDB
emptyDb = empty

compileDb :: [BugInfo] -> BugDB
compileDb = foldl' (flip addBug) emptyDb

-- Add or replace bug info in database
addBug :: BugInfo -> BugDB -> BugDB
addBug info db = HS.insert (bugName info) (bugData info) db

-- Merge two databases
-- Merges tables for any of the bug instead of replacing
unionDb :: BugDB -> BugDB -> BugDB
unionDb = mappend

bugsPerCountry :: BugDB -> [(CountryID, Int)]
bugsPerCountry db =
    map countryRecord countries
        where countries = nub $ concatMap keys (elems db)
              countryRecord country =
                  (country, length $ filter (isJust . HS.lookup country) (elems db))

riskCoefficients :: BugDB -> 
                   [(CountryID, Coefficient)] ->
                   [(Frequency, Coefficient)] ->
                   [(BugID, Coefficient)]
riskCoefficients db countryCoefficients frequencyCoefficients = do
  (bug, bugData) <- toList db
  let s = sum $ 
          map (\ (country, freq) ->
                   let c1 = fromJust $ L.lookup country countryCoefficients
                       c2 = fromJust $ L.lookup freq frequencyCoefficients
                   in c1 * c2) $
          toList bugData
  return (bug, s)

instance Monoid BugDB where
    mempty = emptyDb
    mappend = unionWith mappend

-- Encode/decode functions

-- Normalizes identifier's whitespace
cleanId :: TL.Text -> TL.Text
cleanId = TL.unwords . TL.words . TL.strip

cleanLazyId = cleanId . TL.fromStrict

csv2db :: TL.Text -> Either String BugDB
csv2db csv = csvData2Db <$> parseCsv csv

db2csv :: BugDB -> TL.Text
db2csv = writeCsv . db2CsvData

text2BugInfoParser :: AP.Parser BugInfo
text2BugInfoParser = 
    BugInfo <$> name
            <* many1 endOfLine
            <*> (convertData <$> rows)
            <* many endOfLine
        where 
          name = cleanLazyId <$> takeTill isEndOfLine <?> "Bug name"
          rows = (dataRow `sepBy1` (many1 endOfLine)) <?> "One or more data rows"
          dataRow = (((,) . cleanLazyId) <$> takeTill (== ':') <* char ':' <?> "Frequency")
                    <*> (country `sepBy1` char ',') <?> "One or more countries"
          country = cleanLazyId <$> takeTill (\c -> c == ',' || isEndOfLine c)
                    <?> "Country name"
          convertData :: [(Frequency, [CountryID])] -> BugData
          convertData = fromList . (>>= \row ->
                        zip (snd row) (repeat (fst row)))
                            
text2BugInfo :: TL.Text -> Either String BugInfo
text2BugInfo = AP.eitherResult . AP.parse (text2BugInfoParser <* endOfInput)

coefficientFileParser :: AP.Parser [(TL.Text, Coefficient)]
coefficientFileParser = record `sepBy` endOfLine <* many endOfLine
    where record = (\coef country -> (cleanLazyId country, coef)) <$> 
                   AP.decimal <* many1 (char ' ') 
                   <*> takeTill isEndOfLine

text2Coefficients :: TL.Text -> Either String [(TL.Text, Coefficient)]
text2Coefficients = AP.eitherResult . AP.parse (coefficientFileParser <* endOfInput)

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKeys f = fromList . map (first f) . toList
    
type CSVData = [[TL.Text]]

csvParser :: AP.Parser CSVData
csvParser = csvLine `sepBy` endOfLine
    where csvLine = csvField `sepBy1` char ';'
          csvField = TL.fromStrict <$> takeTill (\c -> c == ';' || isEndOfLine c)

parseCsv :: TL.Text -> Either String CSVData
parseCsv = AP.eitherResult . AP.parse (csvParser <* endOfInput)

writeCsv :: CSVData -> TL.Text
writeCsv = TL.intercalate "\r\n" . map (TL.intercalate ";")

csvData2Db :: CSVData -> BugDB
csvData2Db (header:rest) =
    mconcat $ do
      row <- rest
      let (countryId:valueFields) = row
      (bugId, s) <- filter (("" /=) . snd) $ zip bugIds valueFields
      return $ singleton bugId (singleton countryId s)
        where (_:bugIds) = header

db2CsvData :: BugDB -> CSVData
db2CsvData db = header:rest
    where header = "Регион":map fst dbList
          dbList = toList db
          countryIds = nub $ concatMap (keys . snd) dbList
          rest = map restRow countryIds
          restRow countryId = countryId : map (HS.lookupDefault "" countryId . snd) dbList

-- IO helpers

readDb :: FilePath -> ErrorT String IO BugDB
readDb path = ErrorT $ do
  contents <- readFileUtf8 path
  return $ csv2db contents

readCoefficientFile :: FilePath -> ErrorT String IO [(TL.Text, Coefficient)]
readCoefficientFile path = ErrorT $ do
  contents <- readFileUtf8 path
  return $ text2Coefficients contents

readFileUtf8 :: FilePath -> IO TL.Text
readFileUtf8 path = TLE.decodeUtf8 <$> BL.readFile path

writeFileUtf8 :: FilePath -> TL.Text -> IO ()
writeFileUtf8 path contents = BL.writeFile path $ TLE.encodeUtf8 contents
