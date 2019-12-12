import System.IO
import Control.Monad.IO.Class
import Control.Monad
import Data.List.Split
import Debug.Trace
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

type Storage = Map.Map Int String

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       let zipped = zip [0..] (splitOn "," numbers) in
       doOperations 0 (Map.fromList zipped)
        

doOperations :: Int -> Storage -> IO ()
doOperations n storage = 
    if n >= (length storage -4) then 
        print . mL 1 $ storage
    else do
        let pos1 =  mL (n+1) storage
        let pos1Val = mL pos1 storage
        let pos2 = mL (n+2) storage
        let pos2Val = mL pos2 storage
        let var = mL (n+3) storage
        let varVal = mL var storage
        let opCode = extend . show . abs . mL n $ storage

        case opCode of 
            [_,_,_,3] ->    putStr "Opcode 3> " >> getLine >>= \input ->
                            doOperations (n+2) (mI pos1 (read input) storage)
            [_,_,_,4] -> do putStr $ "\nOpcode 4> " ++ show pos1Val
                            doOperations (n+2) storage

            [_,_,9,9] -> print . mL 1 $ storage
            [z,x,_,t] | t == 1 -> do
                            let val1 = if x == 1 then pos1 else pos1Val
                            let val2 = if z == 1 then pos1 else pos1Val
                            doOperations (n+4) . mI varVal (val1 + val2) $ storage
                      | t == 2 -> do
                            let val1 = if x == 1 then pos1 else pos1Val
                            let val2 = if z == 1 then pos1 else pos1Val
                            doOperations (n+4) . mI varVal (val1 * val2) $ storage
                      | otherwise -> doOperations (n+4) storage


extend :: String -> [Int]
extend n = take (4 - length n) [0,0,0] ++ strToInt n

strToInt :: String -> [Int]
strToInt = map digitToInt

mL :: Int -> Storage -> Int
mL n s = read $ fromJust $ Map.lookup n s

mI :: Int -> Int -> Storage -> Storage
mI n n1 = Map.insert n (show n1)