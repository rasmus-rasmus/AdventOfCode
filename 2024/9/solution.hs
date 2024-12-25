import System.IO  
import Control.Monad
import Data.List
import Data.Char (digitToInt)
import Debug.Trace
import Data.Maybe (fromJust)
import qualified Data.Map as Map


type FileSysMap = Map.Map Int [Int]


getFileSpaces :: String -> [[Int]]
getFileSpaces input = [take (fileAllocations !! i) (repeat i) | i <- [0..length fileAllocations - 1]]
    where fileAllocations = [digitToInt (input !! i) | i <- [0, 2..length input - 1]]

getFreeSpaces :: String -> [Int]
getFreeSpaces input = [digitToInt $ input !! i | i <- [1, 3..length input - 1]]

compactifyFileSystemRecursive :: [[Int]] -> [Int] -> [Int] -> [Int]
compactifyFileSystemRecursive [] _ compact = compact
compactifyFileSystemRecursive (f:[]) _ compact = f ++ compact
compactifyFileSystemRecursive files [] compact = (reverse $ concat files) ++ compact 
compactifyFileSystemRecursive files free compact
    | head free == 0 = compactifyFileSystemRecursive (tail files) (tail free) (head files ++ compact)
    | (length $ last files) == 0 = compactifyFileSystemRecursive (init files) free (compact)
    | otherwise = compactifyFileSystemRecursive ((init files) ++ [init $ last files]) ((head free - 1):(tail free)) ((last $ last files):compact)


-- compactifyFileSystemRecursiveDebug :: [[Int]] -> [Int] -> [Int] -> [Int]
-- compactifyFileSystemRecursiveDebug [] _ compact = compact
-- compactifyFileSystemRecursiveDebug (f:[]) _ compact = f ++ compact
-- compactifyFileSystemRecursiveDebug files [] compact = (reverse $ concat files) ++ compact 
-- compactifyFileSystemRecursiveDebug files free compact
--     | head free == 0 = trace ("Files: " ++ (show files)) $  compactifyFileSystemRecursiveDebug (tail files) (tail free) (head files ++ compact)
--     | (length $ last files) == 0 = trace ("Files: " ++ (show files)) $  compactifyFileSystemRecursiveDebug (tail $ init files) free (head files ++ compact)
--     | otherwise = trace ("Files: " ++ (show files)) $  compactifyFileSystemRecursiveDebug ((init files) ++ [init $ last files]) ((head free - 1):(tail free)) ((last $ last files):compact)


compactifyFileSystem :: [[Int]] -> [Int] -> [Int]
compactifyFileSystem files free = reverse $ compactifyFileSystemRecursive (tail files) free (head files)

getChecksum :: [Int] -> Int
getChecksum filesys = sum $ [i * (filesys !! i) | i <- [0..length filesys - 1]]

-- Part 2

makeFileSysMap :: String -> FileSysMap
makeFileSysMap input = Map.fromList $ foldl 
                                      (\acc idx -> 
                                        ((idx, if idx `mod` 2 == 0 then (files !! (idx `div` 2)) else take (free !! (idx `div` 2)) (repeat (-1))):acc)) 
                                      [] 
                                      [0..length input - 1]
    where files = getFileSpaces input
          free = getFreeSpaces input

getFirstAvailableFreeSpace :: FileSysMap -> Int -> Int
getFirstAvailableFreeSpace fsMap fileIdx = if Map.null filteredMap || length file == 0 then -1 else fst $ Map.findMin $ filteredMap
    where filteredMap = Map.filterWithKey (\k block -> k < fileIdx && any (== -1) block && (length $ filter (== -1) block) >= length file) fsMap
          file = fromJust $ Map.lookup fileIdx fsMap

moveFileToFreeSpace :: FileSysMap -> Int -> Int -> FileSysMap
moveFileToFreeSpace fsMap freeIdx fileIdx = Map.insert fileIdx (take (length file) (repeat (-1))) $ Map.insert freeIdx (preexistingFileBlocks ++ file ++ (take padding (repeat (-1)))) fsMap
    where file = fromJust $ Map.lookup fileIdx fsMap
          preexistingFileBlocks = takeWhile (/= -1) free
          free = fromJust $ Map.lookup freeIdx fsMap
          padding = length free - length file - length preexistingFileBlocks

attemptMoveFile :: FileSysMap -> Int -> FileSysMap
attemptMoveFile fsMap fileIdx = if availIdx /= -1 then moveFileToFreeSpace fsMap availIdx fileIdx else fsMap
    where availIdx = getFirstAvailableFreeSpace fsMap fileIdx

moveAllFiles :: String -> [Int]
moveAllFiles input = concat $ map snd (Map.toList $ foldr (\i acc -> attemptMoveFile acc i) (makeFileSysMap input) [0..length input - 1])

getChecksumPart2 :: [Int] -> Int
getChecksumPart2 filesys = sum $ [i * (nullifiedFileSys !! i) | i <- [0..length filesys - 1]]
    where nullifiedFileSys = map (\i -> if i == -1 then 0 else i) filesys

main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        
        let simpleTest = "13142"

        let test = "2333133121414131402"

        -- Part 1
        print $ getChecksum $ compactifyFileSystem (getFileSpaces contents) (getFreeSpaces contents)

        -- Part 2
        print $ getChecksumPart2 $ moveAllFiles test

        hClose handle

