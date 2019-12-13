import System.IO
import Debug.Trace
import Data.List.Split
import Data.List

data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a)
  deriving (Show, Eq)

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= hGetContents >>= \input ->
       orbits . lines $ input

orbits :: [String] -> IO ()
orbits xs = do
    let orbits = concatMap (splitOn ")") xs
    print ""
    
buildOrbits :: Tree a -> Tree a
buildOrbits (Leaf a) = undefined
buildOrbits (Node a b c)= undefined