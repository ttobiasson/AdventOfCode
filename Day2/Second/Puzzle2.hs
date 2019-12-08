import Functions
import System.IO
import Data.List.Split
import qualified Data.List.Safe as Safe
import Data.Char
import Text.Read
main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       print $ g [(u,v) | u <- [0..99], v <- [0..99]] (splitOn "," numbers)

doOperations :: [String] -> Int -> [String]-> [String]
doOperations [] _ nyaList= nyaList
doOperations list@(x:xs) n nyaList= do
    if (n>= (length list -3)) then doOperations [] n nyaList 
    else do
        s <- (Safe.!!) list (n+1)
        let pos1 = f s
        st <- (Safe.!!) list (n+2)
        let pos2 = f st
        str <- (Safe.!!) list (n+3)
        let var = f str
        num <- (Safe.!!) list (n)

        p1 <- (Safe.!!) list pos1
        let posM1 = f p1
        p2 <- (Safe.!!) list pos2
        let posM2 = f p2
        v1 <- (Safe.!!) list var
        let posMVar = f v1
        
        if (n >= ((length list)-4)) then 
            doOperations [] n nyaList 
            
        else if num == "1" then let newList = setAt posMVar (show(posM1+posM2)) list in (doOperations newList (n+4) newList)

               else if num == "2" then let newList = setAt posMVar (show(posM1*posM2)) list in (doOperations newList (n+4) newList)

                     else if num == "99" then doOperations [] 0 nyaList
                          else doOperations [] 0 ["fel"] 
               
            
f :: String -> Int
f s = read s :: Int

g :: [(Int, Int)] -> [String] -> [String]
g [] list = []
g ((u, v):xs) list = do
    let l1 = setAt 1 (show u) list
    let l2 = setAt 2 (show v) l1
    
    if g' l2 == Just 19690720 then (show u):" ":(show v):[] else g xs list

g' :: [String] -> Maybe Int
g' s = Safe.tail ( doOperations s 0 [] ) >>= Safe.head >>= readMaybe
