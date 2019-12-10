import System.IO
import Control.Monad.IO.Class
import Control.Monad
import Data.List.Split
import Debug.Trace
import Data.Maybe
import qualified Data.Map as Map
main :: IO (Maybe String)
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       let zipped = zip [0..] (splitOn "," numbers) in
       doOperations 0 $ (Map.fromList zipped)
        


doOperations :: Int -> Map.Map Int String -> IO (Maybe String)
doOperations n map = 
    if n >= (length map -4) then 
        pure . Map.lookup 1 $ map
    else do
        let pos1 =  Map.lookup (n+1) map
        let pos1Val = Map.lookup (read $ fromJust pos1) map
        let pos2 = Map.lookup (n+2) map
        let pos2Val = Map.lookup (read $ fromJust pos2) map
        let var = Map.lookup (n+3) map
        let varVal = Map.lookup (read $ fromJust var) map
        let opCode = Map.lookup n map
        let vV = read $ fromJust varVal :: Int
        let p1 = read $ fromJust pos1 :: Int
        let p1v = read $ fromJust pos1Val :: Int
        let p2v = read $ fromJust pos2Val :: Int
     
        case fromJust opCode of 
            "1" -> doOperations (n+4) . Map.insert vV (show(p1v + p2v)) $ map
            "2" -> doOperations (n+4) . Map.insert vV (show(p1v * p2v)) $ map
            "3" -> putStr "Opcode 3> " >> fmap read getLine >>= \input ->
                    doOperations (n+4) (Map.insert p1 (trace input input) $ map)
            "4" -> pure . Map.lookup p1 $ map
            "99" -> pure . Map.lookup 1 $ map
