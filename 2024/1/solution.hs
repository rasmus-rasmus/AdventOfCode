import System.IO  
import Control.Monad
import Data.List

main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = toInt singlewords
        let left = [list !! i | i <- [0..((length list) - 1)], even i]
        let right = [list !! i | i <- [0..((length list) - 1)], odd i]

        let res = sum . map (\(x, y) -> abs (x - y)) . zip (sort left) $ (sort right)
        print res
        
        let similarities = [ (left !! i, length . filter (== (left !! i)) $ right) | i <- [0..((length left) - 1)] ]
        let res2 = sum . map (\(x, y) -> x*y) $ similarities
        print res2
        hClose handle


toInt :: [String] -> [Int]
toInt = map read