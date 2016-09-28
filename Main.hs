{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment
import Control.Monad.Writer
import Data.List
import Data.List.Split
import Debug.Trace

import Codec.Picture

data Cell = Unknown | Empty | Full
  deriving (Eq, Show)

showTable rows = concatMap showRow rows
  where showRow row = "|" ++ map showCell row ++ "|\n"
        showCell Unknown = '?'
        showCell Empty = ' '
        showCell Full = '#'

data KnownCell = KnownEmpty | KnownFull

fromKnown :: KnownCell -> Cell
fromKnown KnownEmpty = Empty
fromKnown KnownFull = Full

isSolved :: [[Cell]] -> Bool
isSolved = not . any (any (== Unknown))

solveGriddler :: [[Int]] -> [[Int]] -> [[Cell]]
solveGriddler rows cols = solveGriddler' (emptyTable (length rows) (length cols)) rows cols
  where solveGriddler' table rowClues colClues 
          = let newTable = reduceTable rowClues colClues table
             in if newTable == table then table else {- trace (showTable table) $ -} solveGriddler' newTable rowClues colClues

reduceTable :: [[Int]] -> [[Int]] -> [[Cell]] -> [[Cell]]
reduceTable rowClues colClues table 
  = let rowReducedTable = zipWith reduceLine rowClues table
     in transpose $ zipWith reduceLine colClues (transpose rowReducedTable)

emptyTable :: Int -> Int -> [[Cell]]
emptyTable height width = replicate height (replicate width Unknown)

reduceLine :: [Int] -> [Cell] -> [Cell]
reduceLine clue row = case lineOptions clue row of
                        [] -> row
                        options -> combineLineOptions options

lineOptions :: [Int] -> [Cell] -> [[KnownCell]]
lineOptions = lineOptions' 0
  where lineOptions' i clues row | sum clues + length clues - 1 > length row + i = [] 
        lineOptions' 0 [] [] = [[]]
        lineOptions' i [clue] [] | i == clue = [[]]
        lineOptions' 0 [] (Full:_) = []
        lineOptions' 0 [] (_:rowrest) = map (KnownEmpty :) $ lineOptions' 0 [] rowrest
        lineOptions' 0 clues (Empty:rowrest) = map (KnownEmpty :) (lineOptions' 0 clues rowrest)
        lineOptions' 0 clues (Unknown:rowrest) = map (KnownEmpty :) (lineOptions' 0 clues rowrest) ++ map (KnownFull :) (lineOptions' 1 clues rowrest)
        lineOptions' i (clue:cluerest) (Empty:rowrest) | i == clue = map (KnownEmpty :) $ lineOptions' 0 cluerest rowrest
        lineOptions' i clues (Full:rowrest) = map (KnownFull :) $ lineOptions' (i+1) clues rowrest
        lineOptions' i clues@(clue:_) (Unknown:rowrest) | i < clue = map (KnownFull :) $ lineOptions' (i+1) clues rowrest
        lineOptions' i (clue:cluerest) (Unknown:rowrest) | i == clue = map (KnownEmpty :) $ lineOptions' 0 cluerest rowrest
        lineOptions' _ _ _ = []

combineLineOptions :: [[KnownCell]] -> [Cell]
combineLineOptions = foldl1 (zipWith combineOption) . map (map fromKnown)

combineOption :: Cell -> Cell -> Cell
combineOption Empty Empty = Empty
combineOption Full Full = Full
combineOption _ _ = Unknown

createClues :: [[KnownCell]] -> ([[Int]],[[Int]])
createClues rows = (map (createClueRow 0) rows, map (createClueRow 0) (transpose rows))
  where createClueRow 0 (KnownEmpty:rest) = createClueRow 0 rest
        createClueRow curr (KnownEmpty:rest) = curr : createClueRow 0 rest
        createClueRow curr (KnownFull:rest) = createClueRow (curr + 1) rest
        createClueRow 0 [] = []
        createClueRow curr [] = [curr]


imageClues :: FilePath -> IO (Maybe ([[Int]],[[Int]]))
imageClues path 
  = do image <- readImage path
       case image of Right img -> testClues $ createClues $ imageToTable img
                     Left err -> do putStrLn err
                                    return Nothing
  where testClues (rows,cols) = let solved = solveGriddler rows cols 
                                 in if isSolved solved then return $ Just (rows,cols) 
                                                       else do putStrLn $ "Undefinit griddler: \n" ++ showTable solved
                                                               return $ Nothing

imageToTable :: DynamicImage -> [[KnownCell]]
imageToTable image
  = let rgbImage = convertRGB8 image
        pixels = execWriter $ imagePixels (\p -> tell [p] >> return p) rgbImage
        cells = map pixelToCell pixels
        rows = chunksOf (imageWidth rgbImage) cells
     in rows
  where pixelToCell (PixelRGB8 r g b) 
          | r + g + b >= 3 * 128 = KnownEmpty
          | otherwise            = KnownFull

main = do [filename] <- getArgs
          print =<< imageClues filename
