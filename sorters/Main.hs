module Main where

import           Data.Char           (isSpace)
import           Data.List           (intercalate)
import           Data.List.Split     (splitOn)
import           Options.Applicative
import           Sorters.BubbleSort
import           Sorters.InsertSort
import           Sorters.MergeSort
import           Sorters.QuickSort
import           Sorters.SelectionSort

data SortersOptions = SortersOptions
    { algorithm           :: String
    , readFileLines       :: Bool
    , valuesSeparator     :: String
    , dataType            :: String
    , ignoreLeadingBlanks :: Bool
    , reverseResult       :: Bool
    , inputs              :: [String] } deriving (Show)

sortersOptions :: Parser SortersOptions
sortersOptions = SortersOptions
    <$> strOption
        ( short 'a'
       <> long "algorithm"
       <> metavar "ALGORITHM"
       <> showDefault
       <> value "merge"
       <> help "Sorting algorithm. Can be one of: bubble, insert, merge, quick." )
    <*> switch
        ( short 'f'
       <> long "read-file-lines"
       <> help "Says to program that input strings are paths to files with data separated by newlines.\
               \ If it's disabled - input strings are data separated by VALUES_SEPARATOR." )
    <*> strOption
        ( short 's'
       <> long "values-separator"
       <> metavar "VALUES_SEPARATOR"
       <> showDefault
       <> value ","
       <> help "Specifies separator for input string if flag -f/--read-file-lines is disabled." )
    <*> strOption
        ( short 't'
       <> long "type"
       <> metavar "TYPE"
       <> showDefault
       <> value "String"
       <> help "Type of data to sort. Can be one of: Int, Integer, Float, Double, String." )
    <*> switch
        ( short 'i'
       <> long "ignore-leading-blanks"
       <> help "Ignore leading blanks in sorting. Works only with TYPE 'String'." )
    <*> switch
        ( short 'r'
       <> long "reverse"
       <> help "Reverse result." )
    <*> many ( argument str
        ( metavar "INPUTS..."
       <> help "Inputs for sorting. See -f/--read-file-lines." ) )

prog :: ParserInfo SortersOptions
prog = info (helper <*> sortersOptions)
      ( fullDesc
     <> header "sorters - concatenate and sort data using various algorithms." )

main :: IO ()
main = do
    options <- execParser prog
    inputData <- getInputData options
    let sorted = sortInputData options inputData
    if reverseResult options then
        putStrLn $ joinSorted $ reverse $ map reverse sorted
    else
        putStrLn $ joinSorted sorted

joinSorted :: [[String]] -> String
joinSorted = intercalate "\n" . map (intercalate "\n")

getInputData :: SortersOptions -> IO [[String]]
getInputData options
    | null $ inputs options = do
        contents <- getContents
        return [lines contents]
    | readFileLines options = do
        files <- mapM readFile $ inputs options
        return $ map lines files
    | otherwise =
        return $ map (splitOn (valuesSeparator options)) $ inputs options

sortInputData :: SortersOptions -> [[String]] -> [[String]]
sortInputData options data_ = case dataType options of
    "Int"     -> map (\d -> map show $ sorter (map read d :: [Int]    )) data_
    "Integer" -> map (\d -> map show $ sorter (map read d :: [Integer])) data_
    "Float"   -> map (\d -> map show $ sorter (map read d :: [Float]  )) data_
    "Double"  -> map (\d -> map show $ sorter (map read d :: [Double] )) data_
    "String"  -> map stringSorter data_
    type_     -> error $ "Unknown data type: " ++ type_
    where
        sorter :: Ord a => [a] -> [a]
        sorter = getSorter $ algorithm options
        stringSorter = getStringSorter options

-- ignoreLeadingBlanksString
newtype ILBString = ILBString { getILBString :: String }

instance Eq ILBString where
    (ILBString a) == (ILBString b) = dropWhile isSpace a == dropWhile isSpace b
    (ILBString a) /= (ILBString b) = dropWhile isSpace a /= dropWhile isSpace b

instance Ord ILBString where
    (ILBString a) `compare` (ILBString b) = dropWhile isSpace a `compare` dropWhile isSpace b

getStringSorter :: SortersOptions -> ([String] -> [String])
getStringSorter options
    | ignoreLeadingBlanks options = map getILBString . sorter . map ILBString
    | otherwise = sorter
    where
        sorter :: Ord a => [a] -> [a]
        sorter = getSorter $ algorithm options

getSorter :: Ord a => String -> [a] -> [a]
getSorter "bubble" = bubbleSort
getSorter "insert" = insertSort
getSorter "merge"  = mergeSort
getSorter "quick"  = quickSort
getSorter algo     = error $ "Unknown algorithm: " ++ algo
