import System.IO  
import Control.Monad
import Data.List
import qualified Data.List.Split as Splt
import Text.Regex

getPair :: String -> (Int, Int)
getPair str = (read first :: Int, read second :: Int)
    where (first, second) = case matchRegexAll (mkRegex "\\|") str of
            Nothing -> ("nothing", "nothing")
            Just (before, _, after, _) -> (before, after)

getOrderingRules :: [String] -> [(Int, Int)]
getOrderingRules lst = map getPair (takeWhile ('|' `elem`) lst)

getUpdates :: [String] ->[[Int]]
getUpdates input = map (toInt . (Splt.splitOn ",")) lst
    where lst = dropWhile ('|' `elem`) input

isLegal :: Int -> Int -> [(Int, Int)] -> Bool
isLegal first second rules = not ((second, first) `elem` rules)

isUpdateLegal :: [Int] -> [(Int, Int)] -> Bool
isUpdateLegal [] _ = True
isUpdateLegal (x:updateRest) rules = foldl (\acc y -> acc && isLegal x y rules) True updateRest && isUpdateLegal updateRest rules

getLegalUpdates :: [[Int]] -> [(Int, Int)] -> [[Int]]
getLegalUpdates updates rules = filter (\update -> (isUpdateLegal update rules)) updates

getIllegalUpdates :: [[Int]] -> [(Int, Int)] -> [[Int]]
getIllegalUpdates updates rules = filter (\update -> not (isUpdateLegal update rules)) updates

-- Part 2
insertLegally :: Int -> [Int] -> [(Int, Int)] -> [Int]
insertLegally toInsert update rules = (takeWhile (\x -> isLegal x toInsert rules) update) ++ toInsert:(dropWhile (\x -> isLegal x toInsert rules) update)
-- insertLegally x [] _ = [x]
-- insertLegally x [y] rules = if isLegal x y rules then [x, y] else [y, x]
-- insertLegally x y:rest rules

fixUpdate :: [Int] -> [(Int, Int)] -> [Int]
fixUpdate [] _ = []
fixUpdate (x:rest) rules = insertLegally x (fixUpdate rest rules) rules

getFixedUpdates :: [[Int]] -> [(Int, Int)] -> [[Int]]
getFixedUpdates updates rules = map (\update -> fixUpdate update rules) (getIllegalUpdates updates rules)

main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let input = filter (/= "") (Splt.splitOn "\n" contents)
        let testContents = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"
        let testInput = filter (/= "") (Splt.splitOn "\n" testContents)

        -- Part 1

        -- print (map (\update -> isUpdateLegal update (getOrderingRules testInput)) (getUpdates testInput))
        -- print (getLegalUpdates (getUpdates testInput) (getOrderingRules testInput))
        -- print (sum (map (\update -> update !! ((length update) `div` 2)) (getLegalUpdates (getUpdates testInput) (getOrderingRules testInput))))

        print (sum (map (\update -> update !! ((length update) `div` 2)) (getLegalUpdates (getUpdates input) (getOrderingRules input))))

        -- Part 2        

        -- print ((getIllegalUpdates (getUpdates testInput) (getOrderingRules testInput)) )
        
        print (sum (map (\update -> update !! ((length update) `div` 2)) (getFixedUpdates (getUpdates input) (getOrderingRules input))))

        hClose handle


toInt :: [String] -> [Int]
toInt = map read