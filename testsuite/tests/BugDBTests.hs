{-# LANGUAGE OverloadedStrings #-}

import           BugDB
import           Control.Applicative
import           Control.Arrow (first, second)
import           Data.Either (isRight)
import           Data.HashMap.Strict (fromList, toList, HashMap)
import qualified Data.HashMap.Strict as HS
import           Data.List (intercalate, foldl')
import qualified Data.Text.Lazy as TL
import           Test.Framework (defaultMain)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit ((@?=), assertBool)
import           Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [ testCase "Parses example bug data file succesfully" test_parsesFile,
          testCase "Parses example data file correctly" test_parsesFileCorrectly,
          testProperty "Generating and then parsing csv gives the same object" prop_csvGenerationAndParsingIsIdentity
        ]

test_parsesFile = do
  contents <- readFileUtf8 "testsuite/data/Desetilinjata.dat"
  assertBool "Failed to parse data file contents" $
    isRight $ text2BugInfo contents

test_parsesFileCorrectly = do
  contents <- readFileUtf8 "testsuite/data/TestInput.dat"
  let Right parseResult = text2BugInfo contents
  parseResult @?= result
      where 
        result = BugInfo "Десятилиньята лепая" $
                 fromList [
                     ("Россия", "Мало")
                   , ("Сибирь", "Мало")
                   , ("Филиппины", "Мало")
                   , ("Албания", "Очень мало")
                   , ("Зимбабве", "Очень мало")
                 ]

prop_csvGenerationAndParsingIsIdentity (ReadableBugDB db) =
  let csv = db2csv db
      parseResult = csv2db csv
      result = Right db == parseResult in
  counterexample ("Failed case's parsed csv: "
                  ++ show (parseCsv csv) 
                  ++ "\nGenerated csv: " ++ show (db2CsvData db)
                  ++ "\nParse result: " ++ show parseResult
                 ) result
  

-- Test set generators

newtype ReadableID = ReadableID {
      unReadableId :: TL.Text
} deriving (Eq, Show)

instance Arbitrary ReadableID where
    arbitrary = (ReadableID . TL.pack . intercalate " ") <$> listOf1 word
        where word = listOf1 $ elements ['а' .. 'я']
    shrink = map wrap . filter (not . null . words) . shrink . unwrap 
        where wrap = ReadableID . TL.pack
              unwrap = TL.unpack . unReadableId
              
newtype ReadableMap = ReadableMap {
      unReadableMap :: HashMap TL.Text TL.Text
} deriving (Eq, Show)

instance Arbitrary ReadableMap where
    arbitrary = wrapReadableMap <$> arbitrary
    shrink = map wrapReadableMap . shrink . unwrapReadableMap

wrapReadableMap :: [(ReadableID, ReadableID)] -> ReadableMap
wrapReadableMap = ReadableMap . fromList . map (first unReadableId . second unReadableId)

unwrapReadableMap :: ReadableMap -> [(ReadableID, ReadableID)]
unwrapReadableMap = map (first ReadableID . second ReadableID) . toList . unReadableMap

newtype ReadableBugInfo = ReadableBugInfo {
      unReadableBugInfo :: BugInfo
}

instance Arbitrary ReadableBugInfo where
    arbitrary = wrapReadableBugInfo <$>
                arbitrary `suchThat` (not . HS.null . unReadableMap . snd)
    shrink = map wrapReadableBugInfo . shrink . unwrapReadableBugInfo
    
wrapReadableBugInfo :: (ReadableID, ReadableMap) -> ReadableBugInfo
wrapReadableBugInfo = (ReadableBugInfo . uncurry BugInfo . 
                       first unReadableId . second unReadableMap)
unwrapReadableBugInfo :: ReadableBugInfo -> (ReadableID, ReadableMap)
unwrapReadableBugInfo (ReadableBugInfo (BugInfo name dataMap)) = 
    (ReadableID name, ReadableMap dataMap)

newtype ReadableBugDB = ReadableBugDB {
      unReadableBugDb :: BugDB
} deriving (Eq, Show)

instance Arbitrary ReadableBugDB where
    arbitrary = (ReadableBugDB . 
                 foldl' (flip addBug) emptyDb . 
                 map unReadableBugInfo) <$> 
                arbitrary `suchThat` (not . null)
    shrink = map wrapReadableBugDb . filter (not . null) . shrink . unwrapReadableBugDb

wrapReadableBugDb :: [(ReadableID, ReadableMap)] -> ReadableBugDB
wrapReadableBugDb = ReadableBugDB . fromList . map (first unReadableId . second unReadableMap)

unwrapReadableBugDb :: ReadableBugDB -> [(ReadableID, ReadableMap)]
unwrapReadableBugDb = map (first ReadableID . second ReadableMap) . toList . unReadableBugDb
