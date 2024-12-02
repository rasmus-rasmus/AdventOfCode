import System.IO  
import Control.Monad
import qualified Data.List.Split as Splt

isSafe :: Int -> Int -> Bool
isSafe x y = x /= y && abs (x-y) < 4


isSafeList :: [Int] -> Bool
isSafeList (x:[]) = True
isSafeList (x:y:[]) = isSafe x y
isSafeList (x:rest) = isSafe x rest0 && ((head rest) - x) * (rest1 - rest0) > 0 && isSafeList rest
                where rest0 = head rest
                      rest1 = rest !! 1

removeNth :: Int -> [] a -> [] a
removeNth = \n -> \list ->
      case n of 
          0 -> tail list
          otherwise -> head list: removeNth (n-1) (tail list)

isSafeListWithDampener :: [Int] -> Bool
isSafeListWithDampener lst = isSafeList lst || foldl (\acc i -> acc || isSafeList (removeNth i lst)) False [0..((length lst) - 1)]

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let lines = map (toInt . words) (Splt.splitOn "\n" contents)
        print (length (filter isSafeList lines))

        print (length (filter isSafeListWithDampener lines))

        hClose handle


toInt :: [String] -> [Int]
toInt = map read