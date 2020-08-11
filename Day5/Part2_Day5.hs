import System.IO
import Control.Monad.IO.Class
import Control.Monad
import Data.List.Split
import qualified Data.List.Safe as Safe
import Debug.Trace
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

type Storage = Map.Map Int String

--https://adventofcode.com/2019/day/5
main :: IO ()
main = openFile "inputDay5Part2.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       let zipped = zip [0..] (splitOn "," numbers) in
       evalOpcodes 0 $ (Map.fromList zipped)
        
--The specified functionality implemented here, the operations are dependent on the description
--in the AoC website.
evalOpcodes :: Int -> Storage -> IO ()
evalOpcodes n storage = do
    let pos1 =  lookup' (n+1) storage
    let pos1Val = lookup' pos1 storage
    let pos2 = lookup' (n+2) storage
    let pos2Val = lookup' pos2 storage
    let var = lookup' (n+3) storage
    let varVal = lookup' var storage
    let opCode = extend . show . abs . lookup' n $ storage

    case opCode of 
        [_,_,_,3] ->    putStr "Opcode 3> " >> getLine >>= \input ->
                        evalOpcodes (n+2) (insert' pos1 (read input) storage)
        [_,_,_,4] -> do putStr $ "\nOpcode 4> " ++ show pos1Val ++ "\n"
                        evalOpcodes (n+2) storage

        [_,_,9,9] -> return ()
        [z,x,_,t] | t == 1 -> do
                        let val1 = if x == 1 then pos1 else pos1Val
                        let val2 = if z == 1 then pos2 else pos2Val
                        evalOpcodes (n+4) . insert' var (val1 + val2) $ storage
                  | t == 2 -> do
                        let val1 = if x == 1 then pos1 else pos1Val
                        let val2 = if z == 1 then pos2 else pos2Val
                        evalOpcodes (n+4) . insert' var (val1 * val2) $ storage
                  | t == 5 -> do
                        let val1 = if x == 1 then pos1 else pos1Val
                        let val2 = if z == 1 then pos2 else pos2Val
                        let newN = if val1 /= 0 then val2 else (n+3)
                        evalOpcodes newN storage
                  | t == 6 -> do
                        let val1 = if x == 1 then pos1 else pos1Val
                        let val2 = if z == 1 then pos2 else pos2Val
                        let newN = if val1 == 0 then val2 else (n+3)
                        evalOpcodes newN storage
                  | t == 7 -> do
                        let val1 = if x == 1 then pos1 else pos1Val
                        let val2 = if z == 1 then pos2 else pos2Val
                        let store = if val1 < val2 then insert' var 1 storage 
                                                   else insert' var 0 storage
                        evalOpcodes (n+4) store
                  | t == 8 -> do
                        let val1 = if x == 1 then pos1 else pos1Val
                        let val2 = if z == 1 then pos2 else pos2Val
                        let store = if val1 == val2 then insert' var 1 storage 
                                                    else insert' var 0 storage
                        evalOpcodes (n+4) store                        
                


--Used to make all the opCode-lists the same length
extend :: String -> [Int]
extend n = take (4 - length n) [0,0,0] ++ map digitToInt n

lookup' :: Int -> Storage -> Int
lookup' n s = read $ fromJust $ Map.lookup n s

insert' :: Int -> Int -> Storage -> Storage
insert' n n1 = Map.insert n (show n1)
