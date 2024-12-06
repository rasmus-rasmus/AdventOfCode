import System.IO  
import Control.Monad
import Data.List
import qualified Data.List.Split as Splt



boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

isValid :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Bool
isValid input (loc0, loc1) (dir0, dir1) wordLength = endRowIdx >= 0 
                                                   && endRowIdx < length input 
                                                   && endColIdx >= 0 
                                                   && endColIdx < length (input !! endRowIdx) 
    where endRowIdx = loc0 + dir0*(wordLength - 1)
          endColIdx = loc1 + dir1*(wordLength - 1)

getDirectedWord :: [String] -> (Int, Int) -> (Int, Int) -> Int -> String
getDirectedWord input (loc0, loc1) (dir0, dir1) wordLength = [ (input !! (loc0 + dir0*i)) !! (loc1 + dir1*i) | i <- [0..(wordLength-1)] ]

hasSolutionDirected :: [String] -> String -> (Int, Int) -> (Int, Int) -> Bool
hasSolutionDirected input keyWord loc dir = if (isValid input loc dir (length keyWord)) 
                                            then (getDirectedWord input loc dir (length keyWord)) == keyWord 
                                            else False

numSolutionsPos :: [String] -> String -> (Int, Int) -> Int
numSolutionsPos input keyWord loc = north + northEast + east + southEast + south + southWest + west + northWest
    where north = if hasSolutionDirected input keyWord loc (-1, 0) then 1 else 0
          northEast = if hasSolutionDirected input keyWord loc (-1, 1) then 1 else 0
          east = if hasSolutionDirected input keyWord loc (0, 1) then 1 else 0
          southEast = if hasSolutionDirected input keyWord loc (1, 1) then 1 else 0
          south = if hasSolutionDirected input keyWord loc (1, 0) then 1 else 0
          southWest = if hasSolutionDirected input keyWord loc (1, -1) then 1 else 0
          west = if hasSolutionDirected input keyWord loc (0, -1) then 1 else 0
          northWest = if hasSolutionDirected input keyWord loc (-1, -1) then 1 else 0



numSolutionsRow :: [String] -> String -> Int -> Int
numSolutionsRow input keyWord row = foldl (\acc i -> acc + numSolutionsPos input keyWord (row, i)) 0 [0..((length (input !! row)) - 1)]

numSolutions :: [String] -> String -> Int
numSolutions input keyWord = foldl (\acc i -> acc + numSolutionsRow input keyWord i) 0 [0..((length input) - 1)]

-- Part 2

hasSolutionDirectedX :: [String] -> String -> ((Int, Int), (Int, Int)) -> Bool
hasSolutionDirectedX input keyWord (loc, dir) = if (isValid input loc dir (length keyWord)) 
                                                then (getDirectedWord input loc dir (length keyWord)) `elem` [keyWord, (reverse keyWord)]
                                                else False

getCrossedLocAndDir :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
getCrossedLocAndDir (loc0, loc1) (dir0, dir1) = ((loc0, loc1 + 2*dir1), (dir0, -dir1))

numSolutionsPosX :: [String] -> (Int, Int) -> Int
numSolutionsPosX input loc = boolToInt northEast + boolToInt southEast + boolToInt southWest + boolToInt northWest
      where northEast = if hasSolutionDirected input "MAS" loc (-1, 1) then hasSolutionDirectedX input "MAS" (getCrossedLocAndDir loc (-1, 1) ) else False
            southEast = if hasSolutionDirected input "MAS" loc (1, 1) then hasSolutionDirectedX input "MAS" (getCrossedLocAndDir loc (1, 1) ) else False
            southWest = if hasSolutionDirected input "MAS" loc (1, -1) then hasSolutionDirectedX input "MAS" (getCrossedLocAndDir loc (1, -1)) else False
            northWest = if hasSolutionDirected input "MAS" loc (-1, -1) then hasSolutionDirectedX input "MAS" (getCrossedLocAndDir loc (-1, -1) ) else False

numSolutionsRowX :: [String] -> Int -> Int
numSolutionsRowX input row = foldl (\acc i -> acc + numSolutionsPosX input (row, i)) 0 [0..((length (input !! row)) - 1)]

numSolutionsX :: [String] -> Int
numSolutionsX input = (foldl (\acc i -> acc + numSolutionsRowX input i) 0 [0..((length input) - 1)]) `div` 2

main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        -- print contents
        let test = ["MMMSXXMASM", 
                    "MSAMXMSMSA", 
                    "AMXSXMAAMM", 
                    "MSAMASMSMX", 
                    "XMASAMXAMM", 
                    "XXAMMXXAMA", 
                    "SMSMSASXSS", 
                    "SAXAMASAAA", 
                    "MAMMMXMMMM", 
                    "MXMXAXMASX"]

        let input = Splt.splitOn "\n" contents
        
        print (numSolutions input "XMAS")

        print (numSolutionsX input)
        
        hClose handle


toInt :: [String] -> [Int]
toInt = map read