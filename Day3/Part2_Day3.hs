import System.IO
import Data.List.Split
import qualified Data.List.Safe as Safe
import Debug.Trace
import Data.List
import Data.List.Unique
import qualified Data.Map as Map
--https://adventofcode.com/2019/day/3

main :: IO ()
main = openFile "inputDay3Part2.txt" ReadMode >>= --Open the file containing the input
       hGetContents >>= \numbers -> 
       let answer = manager numbers [[(0,0)]] in
--Prints the answer by swapping the order of the elements so that the min function
--can be used to find the smallest number in the list.
       print $ foldr1 min $ filter (\(n, _) -> n > 0) $ fmap (\(t, n) -> (n,t)) $ answer

--Here we build the sets of data needed for the comparison that is done in the comparePaths
--function.
manager :: String -> [[(Int, Int)]] -> [((Int, Int), Int)]
manager s l = do
    let dataPoints = map (splitOn "," ) $ lines s
    let intersections = repeated $ concatMap (g . f [[(0,0)]] ) dataPoints
    let points = map (concat . f [[(0,0)]]) dataPoints
    let n1 = buildLengths (head points) intersections []
    let n2 = buildLengths (last points) intersections []
    comparePaths n1 (Map.fromList n2)

--This function checks if the point is an intersection, if it is the length to that intersection
--is added to the list called block. This way we reduce the complexity and save A LOT of time.
buildLengths :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
buildLengths coords [] block = block
buildLengths coords ((x, y):xs) block = 
    if elem (x, y) coords then do
                las <- Safe.last( splitOn [(x, y)] coords )
                let two = length las
                buildLengths coords xs (((x, y), two):block)
    else buildLengths coords xs block

--Find coordinates that exists in both wire's paths. It uses a Map to search for a coordinate
--in the path of the other wire. It saves all the recurring coordinates in a list and adds distances.
comparePaths :: [((Int, Int), Int)] -> Map.Map (Int, Int) Int -> [((Int, Int), Int)]
comparePaths [] s = []
comparePaths (((x, y), z):xs) cs =
    case Map.lookup (x,y) cs of
        Just a -> (((x, y), a+z):(comparePaths xs cs))
        Nothing -> comparePaths xs cs

----Helper-function to remove duplicate coordinates and concatinate the paths
g :: [[(Int, Int)]] -> [(Int, Int)]
g = unique . concat  


--Build up a record of every coordinate the wires have passed. The function does this by looking at 
--one coordinate at a time, and updating x and y accordingly
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
