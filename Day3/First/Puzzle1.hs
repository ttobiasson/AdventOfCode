import System.IO
import Data.List.Split
import qualified Data.List.Safe as Safe
import Data.Char
import Debug.Trace
import Data.List
import Data.List.Unique
import qualified Text.Read as SRead

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers -> 
       let answer = ( h numbers [[(0,0)]] ) in
       print $ foldr1 min $ filter (> 0) $ map (\(x, y) -> (abs x) + (abs y) ) answer

h :: String -> [[(Int, Int)]] -> [(Int, Int)]
h s l = do
    let dataPoints = map (splitOn "," ) $ lines s
    repeated $ concat $ map g $ map (f [[(0,0)]] ) dataPoints


g :: [[(Int, Int)]] -> [(Int, Int)]
g l = unique $ concat l

f :: [[(Int, Int)]] -> [String] ->[[(Int, Int)]]
f  list [] = list
f  list (s:ss)= do
        n <- Safe.tail s
        let num = read n :: Int
        (x, y) <- Safe.head =<< Safe.head list
        case (Safe.head s) of
                "U" -> (f ( reverse[(x, z) | z <- [(y+1)..(num+y)]]:list ) ss )
                "D" -> (f ( [(x, z) | z <- [(y-num)..(y-1)]]:list        ) ss )
                
                "L" -> (f ( [(z, y) | z <- [(x-num)..(x-1)]]:list        ) ss )
                "R" -> (f ( reverse[(z, y) | z <- [(x+1)..(num+x)]]:list ) ss )