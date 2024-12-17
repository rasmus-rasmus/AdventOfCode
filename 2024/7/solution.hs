import System.IO  
import Control.Monad
import Data.List
import qualified Data.List.Split as Splt

parseEquation :: String -> [Int]
parseEquation input = (read (init lhs) :: Int):(toInt rhs)
    where lhs = head splitList
          rhs = tail splitList
          splitList = Splt.splitOn " " input

equationSatisfied :: [Int] -> Bool
equationSatisfied (x:y:[]) = x == y
equationSatisfied (x:rest) = (equationSatisfied ((x - last rest):(init rest))) 
                             || ( x `mod` last rest == 0 && equationSatisfied ((x `div` last rest):(init rest)))

getValidEquationResults :: [[Int]] -> [Int]
getValidEquationResults eqs = map head $ filter equationSatisfied eqs

getValidEquations :: [[Int]] -> [[Int]]
getValidEquations = filter equationSatisfied

-- Part 2

getNTail :: Int -> Int -> Int
getNTail theInt n = read (drop n $ show theInt) :: Int
        where l = length $ show theInt

getNHead :: Int -> Int -> Int
getNHead theInt n = read (take n $ show theInt) :: Int

equationSatisfiedWithCat :: [Int] -> Bool
equationSatisfiedWithCat (x:y:[]) = x == y
equationSatisfiedWithCat (x:rest) = (equationSatisfiedWithCat ((x - last rest):(init rest))) 
                             || ( x `mod` last rest == 0 && equationSatisfiedWithCat ((x `div` last rest):(init rest)))
                             || (lenX > lenLast && getNTail x (lenX - lenLast) == last rest && equationSatisfiedWithCat ((getNHead x (lenX - lenLast)):(init rest)))
        where lenX = length $ show x
              lenLast = length $ show (last rest)

getValidEquationResultsWithCat :: [[Int]] -> [Int]
getValidEquationResultsWithCat eqs = map head $ filter equationSatisfiedWithCat eqs


main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let input = Splt.splitOn "\n" contents

        -- print $ getValidEquations (map parseEquation input)
        
        let testInput = ["190: 10 19", 
                         "3267: 81 40 27", 
                         "83: 17 5", 
                         "156: 15 6", 
                         "7290: 6 8 6 15", 
                         "161011: 16 10 13", 
                         "192: 17 8 14", 
                         "21037: 9 7 18 13", 
                         "292: 11 6 16 20"]

        -- Part 1
        print $ sum $ getValidEquationResults (map parseEquation input)

        -- Part 2
        print $ sum $ getValidEquationResultsWithCat (map parseEquation input)

        hClose handle


toInt :: [String] -> [Int]
toInt = map read