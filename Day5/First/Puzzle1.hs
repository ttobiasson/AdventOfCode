import System.IO
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map
main :: IO ()
main = 
       openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       let zipped = zip [0..] $ (splitOn "," numbers) in
       print $ doOperations 0 (Map.fromList zipped)

doOperations ::  Int -> Map.Map Int [Char] -> Maybe [Char]
doOperations n map = do

    (Just pos1) <- Map.lookup (n+1) map
    (Just pos1Val) <- Map.lookup pos1 map
    (Just pos2) <- Map.lookup (n+2) map
    (Just pos2Val) <- Map.lookup pos2 map
    (Just var) <- Map.lookup (n+3) map
    (Just varVal) <- Map.lookup var map

    Just opCode <- Map.lookup n map

    case opCode of 
        "1" -> let newMap = Map.insert varVal (pos1Val + pos2Val) map in doOperations (n+4) newMap
        "2" -> let newMap = Map.insert varVal (pos1Val * pos2Val) map in doOperations (n+4) newMap
        "99" -> doOperations n Map.empty
