import System.IO
import Data.List.Split
import qualified Data.List.Safe as Safe
import Debug.Trace
import Data.List
import Data.List.Unique
import qualified Data.Map as Map
--https://adventofcode.com/2019/day/3

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= --Open the file containing the input
       hGetContents >>= \numbers -> 
       let answer = h numbers [[(0,0)]] in
       print $ foldr1 min $ filter (\(n, _) -> n > 0) $ fmap (\(t, n) -> (n,t)) $ answer

h :: String -> [[(Int, Int)]] -> [((Int, Int), Int)]
h s l = do
    let dataPoints = map (splitOn "," ) $ lines s
    let intersections = repeated $ concatMap (g . f [[(0,0)]] ) dataPoints
    let points = map (concat . f [[(0,0)]]) dataPoints
    let n1 = fseek (head points) intersections []
    let n2 = fseek (last points) intersections []
    ffind n1 (Map.fromList n2)

fseek :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
fseek pa [] block = block
fseek pa ((x, y):xs) block = 
    if elem (x, y) pa then do
                las <- Safe.last( splitOn [(x, y)] pa )
                let two = length las
                fseek pa xs (((x, y), two):block)
    else fseek pa xs block

ffind :: [((Int, Int), Int)] -> Map.Map (Int, Int) Int -> [((Int, Int), Int)]
ffind [] s = []
ffind (((x, y), z):xs) cs =
    case Map.lookup (x,y) cs of
        Just a -> (((x, y), a+z):(ffind xs cs))
        Nothing -> ffind xs cs

--Remove duplate coordinates and concatinate the paths
g :: [[(Int, Int)]] -> [(Int, Int)]
g = unique . concat  


--Build up a record of every coordinate the wires both have passed
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
