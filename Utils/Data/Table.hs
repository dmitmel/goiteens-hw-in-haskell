module Data.Table where

import           Data.List       (find)
import           Data.List.Utils
import           Data.Maybe      (fromJust)

data Table a = Table { rows :: Int, cols :: Int, grid :: [[a]] } deriving (Show, Eq, Ord, Read)

instance Functor Table where
    fmap f (Table rows_ cols_ grid_) = Table rows_ cols_ $ map (map f) grid_

instance Foldable Table where
    foldMap f (Table _ _ grid) = foldMap (foldMap f) grid

(#-+) :: Table a -> (Int, a) -> Table a
table                     #-+ (0,       _  ) = table
(Table rows_ cols_ grid_) #-+ (newRows, def) = Table (rows_ + newRows) cols_ $ grid_ ++ replicate newRows (replicate cols_ def)

(#|+) :: Table a -> (Int, a) -> Table a
table                     #|+ (0,       _  ) = table
(Table rows_ cols_ grid_) #|+ (newCols, def) = Table rows_ (cols_ + newCols) $ map (\row -> row ++ replicate newCols def) grid_

tableToRows :: Table a -> [[a]]
tableToRows = grid

tableToCols :: Table a -> [[a]]
tableToCols table = toBlocks (rows table) $ map (table #) rowsAndCols
    where rowsAndCols = [ (row, col) | col <- [0..cols table - 1], row <- [0..rows table - 1] ]

(#=) :: Table a -> ((Int, Int), a) -> Table a
(Table rows_ cols_ grid_) #= ((row, col), x)
    | row < 0      = error $ "Data.Table.(#): row (" ++ show row ++ ") is smaller than 0"
    | col < 0      = error $ "Data.Table.(#): column (" ++ show col ++ ") is smaller than 0"
    | row > maxRow = error $ "Data.Table.(#): row (" ++ show row ++ ") is out of bounds (max row = " ++ show maxRow ++ ")"
    | col > maxCol = error $ "Data.Table.(#): coolumn (" ++ show col ++ ") is out of bounds (max column = " ++ show maxCol ++ ")"
    | otherwise     = Table rows_ cols_ $ grid_ !!= (row, grid_ !! row !!= (col, x))
    where maxRow = rows_ - 1
          maxCol = cols_ - 1

(#) :: Table a -> (Int, Int) -> a
(Table rows_ cols_ grid_) # (row, col)
    | row < 0      = error $ "Data.Table.(#): row (" ++ show row ++ ") is smaller than 0"
    | col < 0      = error $ "Data.Table.(#): column (" ++ show col ++ ") is smaller than 0"
    | row > maxRow = error $ "Data.Table.(#): row (" ++ show row ++ ") is out of bounds (max row = " ++ show maxRow ++ ")"
    | col > maxCol = error $ "Data.Table.(#): coolumn (" ++ show col ++ ") is out of bounds (max column = " ++ show maxCol ++ ")"
    | otherwise     = grid_ !! row !! col
    where maxRow = rows_ - 1
          maxCol = cols_ - 1

toTableHorizontal :: Int -> [a] -> Table a
toTableHorizontal cols_ xs
    | len `mod` cols_ == 0 = Table rows_ cols_ $ toBlocks cols_ xs
    | otherwise            = error $ "Data.Table.toTableHorizontal: not enough elements to fill row to end \
                                     \(total = " ++ show len ++ ")"
    where len   = length xs
          rows_ = len `div` cols_

toTableHorizontalDefault :: Int -> a -> [a] -> Table a
toTableHorizontalDefault rowSize def xs = toTableHorizontal rowSize $ xs ++ replicate missingCount def
    where missingCount = fromJust $ find (\n -> (len + n) `mod` rowSize == 0) [0..]
          len = length xs

toTableVertical :: Int -> [a] -> Table a
toTableVertical rows_ xs
    | len `mod` rows_ == 0 = Table rows_ cols_ $ toBlocks cols_ $ map (\(row, col) -> xs !! (row + rows_ * col)) rowsAndCols
    | otherwise            = error $ "Data.Table.toTableHorizontal: not enough elements to fill column to end \
                                     \(total = " ++ show len ++ ")"
    where len         = length xs
          cols_       = len `div` rows_
          rowsAndCols = [ (row, col) | row <- [0..rows_ - 1], col <- [0..cols_ - 1]]

toTableVerticalDefault :: Int -> a -> [a] -> Table a
toTableVerticalDefault colSize def xs = toTableVertical colSize $ xs ++ replicate missingCount def
    where missingCount = fromJust $ find (\n -> (len + n) `mod` colSize == 0) [0..]
          len = length xs

(#-) :: Table a -> Int -> [a]
(Table rows_ _ grid_) #- row
    | row < 0      = error $ "Data.Table.(#-): row (" ++ show row ++ ") is smaller than 0"
    | row > maxRow = error $ "Data.Table.(#-): row (" ++ show row ++ ") is out of bounds (max row = " ++ show maxRow ++ ")"
    | otherwise    = grid_ !! row
    where maxRow = rows_ - 1

(#-=) :: Table a -> (Int, [a]) -> Table a
(Table rows_ cols_ grid_) #-= (row, rowVals)
    | row < 0                = error $ "Data.Table.(#=-): row (" ++ show row ++ ") is smaller than 0"
    | row > maxRow           = error $ "Data.Table.(#=-): row (" ++ show row ++ ") is out of bounds (max row = " ++ show maxRow ++ ")"
    | length rowVals < cols_ = error $ "Data.Table.(#=-): not enough values in new row (missing = " ++ show (cols_ - length rowVals) ++ ")"
    | length rowVals > cols_ = error $ "Data.Table.(#=-): too many values in new row (not needed = " ++ show (length rowVals - cols_) ++ ")"
    | otherwise              = Table rows_ cols_ $ grid_ !!= (row, rowVals)
    where maxRow = rows_ - 1

(#|) :: Table a -> Int -> [a]
(Table _ cols_ grid_) #| col
    | col < 0      = error $ "Data.Table.(#|): column (" ++ show col ++ ") is smaller than 0"
    | col > maxCol = error $ "Data.Table.(#|): column (" ++ show col ++ ") is out of bounds (max column = " ++ show maxCol ++ ")"
    | otherwise    = foldr (\row colVals -> (row !! col):colVals) [] grid_
    where maxCol = cols_ - 1

(#|=) :: Table a -> (Int, [a]) -> Table a
(Table rows_ cols_ grid_) #|= (col, colVals)
    | col < 0                = error $ "Data.Table.(#|): column (" ++ show col ++ ") is smaller than 0"
    | col > maxCol           = error $ "Data.Table.(#|): column (" ++ show col ++ ") is out of bounds (max column = " ++ show maxCol ++ ")"
    | length colVals < rows_ = error $ "Data.Table.(#=-): not enough values in new column (missing = " ++ show (rows_ - length colVals) ++ ")"
    | length colVals > rows_ = error $ "Data.Table.(#=-): too many values in new column (not needed = " ++ show (length colVals - rows_) ++ ")"
    | otherwise              = Table rows_ cols_ $ zipWith (\row colVal -> row !!= (col, colVal)) grid_ colVals
    where maxCol = cols_ - 1
