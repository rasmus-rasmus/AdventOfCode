import System.IO  
import Control.Monad
import Text.Regex

getAllMatches :: String -> String -> [String]
getAllMatches regex str = case matchRegexAll (mkRegex regex) str of
    Nothing -> []
    Just (_, matched, after, _) -> matched : getAllMatches regex after

performMultiplication :: String -> Int
performMultiplication str = (read (head match) :: Int) * (read (last match) :: Int)
    where match = getAllMatches "([0-9]+)" str

performAllMultiplications :: String -> [Int]
performAllMultiplications str = map performMultiplication allMatches
    where allMatches = getAllMatches "(mul\\([0-9]+,[0-9]+\\))" str

stripExcessiveDont :: String -> String
stripExcessiveDont str = case matchRegexAll (mkRegex "don't\\(\\)") str of
    Nothing -> str
    Just (before, _, _, _) -> before ++ "don't()"

getDoBlocks :: String -> [String]
getDoBlocks str = case matchRegexAll (mkRegex "do\\(\\)") str of
    Nothing -> []
    Just (_, _, afterDo, _) -> doBlock : getDoBlocks afterDont
        where (doBlock, afterDont) = case matchRegexAll (mkRegex "don't\\(\\)") afterDo of
                Nothing -> (afterDo, "")
                Just (before, _, after, _) -> (before, after)

performAllDo :: String -> Int
performAllDo str = sum (map (sum . performAllMultiplications) dos)
    where dos = getDoBlocks ("do()" ++ str)

main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let test = "ffjjfmul(1,1)dkd f\njf mul(1,2)dkjdon't()mul(1,3)sdo()mul(1,4)don't()dfkjmul(1,5)mul(1,6)don't()dfkjdo()mul(11,10)don't()"
        let test2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
        
        print (sum . performAllMultiplications $ contents)

        print (performAllDo contents)
        
        hClose handle


toInt :: [String] -> [Int]
toInt = map read