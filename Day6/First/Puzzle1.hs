import System.IO
import Debug.Trace
import Data.List.Split
import Data.List
import qualified Data.Map as Map

type Abel = Map.Map String Int

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= hGetContents >>= \input ->
       print $ foldl (\a x -> a + ((fromIntegral $ x) / 3) ) 0 $ orbits ( map (splitOn ")") $ lines $ input ) Map.empty

orbits :: [[String]] -> Abel -> Abel
orbits [] abel = abel
orbits ((x:y:xs):xss) abel = do 
    let val = orbits' x 1 abel
    orbits xss val

orbits' :: String -> Int -> Abel -> Abel
orbits' = Map.insertWith (+)