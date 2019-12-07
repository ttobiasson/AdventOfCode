import Functions
import System.IO
import Data.List.Split
import Data.Char
import Text.Read
main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       print $ head $ g [(u,v) | u <- [0..50], v <- [0..50]] (splitOn "," numbers)

doOperations :: [String] -> Int -> [String]-> [String]
doOperations [] _ nyaList= nyaList
doOperations list n nyaList= do
    let pos1 = f(list !! (n+1))
    let pos2 = f(list !! (n+2))
    let var  = f(list !! (n+3))
    if (n >= ((length list)-4)) then 
        doOperations [] n nyaList 
    
    else if (list !! n) == "1" then let newList = setAt (f(list !! var)) (show((f(list !! pos1))+(f(list !! pos2)))) list in (doOperations newList (n+4) newList)

            else if (list !! n) == "2" then let newList = setAt (f(list !! var)) (show((f(list !! pos1))*(f(list !! pos2)))) list in (doOperations newList (n+4) newList)

                 else if (list !! n) == "99" then doOperations [] 0 nyaList
                      else doOperations [] 0 ["fel"] 
               
            
f :: String -> Int
f s = read s :: Int

g :: [(Int, Int)] -> [String] -> [String]
g [] list = ["Nothing","Nothing"]
g ((u, v):xs) list = do
    let l1 = setAt 1 (show u) list
    let l2 = setAt 2 (show v) l1
    
    if g' l2 == Just 19690720 then (show u):" ":(show v):[] else g xs list

g' :: [String] -> Maybe Int
g' s = readMaybe ( head (tail ( doOperations s 0 [] )))
