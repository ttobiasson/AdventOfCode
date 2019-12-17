import System.IO
import Debug.Trace
import Data.List.Split
import Data.List
import qualified Data.Map as Map

type Abel = Map.Map String String

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= hGetContents >>= \input ->
       print $ foldl (\a x -> a + ((fromIntegral $ length $ x) / 3) ) 0 $ orbits ( map (splitOn ")") $ lines $ input ) Map.empty

orbits :: [[String]] -> Abel -> Abel
orbits [] abel = abel
orbits ((x:y:xs):xss) abel = do 
    let val = orbits' x y abel
    orbits xss val

orbits' :: String -> String -> Abel -> Abel
orbits' x y z = Map.insertWith (++) x y z