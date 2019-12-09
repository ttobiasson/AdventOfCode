import System.IO
import Data.List.Split
import qualified Data.List.Safe as Safe
import Debug.Trace
import Data.List
import Data.List.Unique
import qualified Data.Map as Map

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers -> 
       let answer = h numbers [[(0,0)]] in
       print $ foldr1 min $ filter (\(n, (a, b)) -> n > 0) $ fmap (\((a, b), n) -> (n,(a, b))) $ answer

h :: String -> [[(Int, Int)]] -> [((Int, Int), Int)]
h s l = do
    let dataPoints = map (splitOn "," ) $ lines s
    let intersections = repeated $ concat $ map g $ map (f [[(0,0)]] ) dataPoints
    let points = map g' $ map (f [[(0,0)]] ) dataPoints
    let n1 = fseek (head $ points) intersections []
    let n2 = fseek (last $ points) intersections []
    ffind (n1) (Map.fromList n2)

fseek :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
fseek pa [] block = block
fseek pa ((x, y):xs) block = do
    case elem (x, y) pa of
        True -> do
                las <- Safe.last( splitOn [(x, y)] pa )
                let two = length las
                fseek pa xs ((((x, y), two)):block)
        False -> fseek pa xs block

ffind :: [((Int, Int), Int)] -> (Map.Map (Int, Int) Int) -> [((Int, Int), Int)]
ffind [] s = []
ffind (((x, y), z):xs) cs = do
    case Map.lookup (x,y) cs of
        Just a -> (((x, y), a+z):(ffind xs cs))
        Nothing -> ffind xs cs

g' :: [[(Int, Int)]] -> [(Int, Int)]
g' l = concat l

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