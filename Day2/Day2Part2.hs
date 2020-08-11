import Functions
import System.IO
import Data.List.Split
import qualified Data.List.Safe as Safe
import Data.Char
import Text.Read
--https://adventofcode.com/2019/day/2

main :: IO ()
main = openFile "inputDay2Part2.txt" ReadMode >>= --Open the file containing the input  
       hGetContents >>= \numbers ->
       print $ checkTuples [(u,v) | u <- [0..99], v <- [0..99]] (splitOn "," numbers)

doOperations :: [String] -> Int -> [String]-> [String]
doOperations [] _ nyaList= nyaList
doOperations list@(x:xs) n nyaList= 
        --Depending on the input number's value, we do the specified operation
    if (n>= (length list -3)) then doOperations [] n nyaList 
    else do
        s <- (Safe.!!) list (n+1)
        let pos1 = toInt s
        st <- (Safe.!!) list (n+2)
        let pos2 = toInt st
        str <- (Safe.!!) list (n+3)
        let var = toInt str
        num <- (Safe.!!) list n

        p1 <- (Safe.!!) list pos1
        let posM1 = toInt p1
        p2 <- (Safe.!!) list pos2
        let posM2 = toInt p2
        v1 <- (Safe.!!) list var
        let posMVar = toInt v1
        
        if (n >= ((length list)-4)) then 
            doOperations [] n nyaList 
            
        else if num == "1" then let newList = setAt posMVar (show(posM1+posM2)) list in (doOperations newList (n+4) newList)

               else if num == "2" then let newList = setAt posMVar (show(posM1*posM2)) list in (doOperations newList (n+4) newList)

                     else if num == "99" then doOperations [] 0 nyaList
                          else doOperations [] 0 ["fel"] 
               
            
toInt :: String -> Int
toInt s = read s :: Int

checkTuples :: [(Int, Int)] -> [String] -> [String]
checkTuples [] list = []
checkTuples ((u, v):xs) list = do
    let l1 = setAt 1 (show u) list
    let l2 = setAt 2 (show v) l1
    --We look for the answer by checking if the pair of ints produced the desired output
    if checkTuple l2 == Just 19690720 then (show u):" ":(show v):[] else checkTuples xs list

checkTuple :: [String] -> Maybe Int
checkTuple s = Safe.tail ( doOperations s 0 [] ) >>= Safe.head >>= readMaybe
