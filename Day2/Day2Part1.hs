--import Functions
import System.IO
import Functions
import Functions2
--import Functions2
--https://adventofcode.com/2019/day/2

main :: IO ()
main = openFile "inputDay2Part1.txt" ReadMode >>= --Open the file containing the input  
       hGetContents >>= \numbers ->
       print (doOperations (splitOn "," numbers) 0 [] )

doOperations :: [String] -> Int -> [String]-> [String]
doOperations [] _ nyaList= nyaList
doOperations list n nyaList= do
--Initialize the variables with the positions to evaluate
    let pos1 = f(list !! (n+1))
    let pos2 = f(list !! (n+2))
    let var  = f(list !! (n+3))
    if (n >= ((length list)-3)) then 
        doOperations [] n nyaList
        
    --Depending on what the number at position n is, we change the position that corresponds
    --to the value of the variable "var". Var is changed recursively, as is n.
    else if (list !! n) == "1" then let newList = setAt (f(list !! var)) (show((f(list !! pos1))+(f(list !! pos2)))) list in (doOperations newList (n+4) newList)

            else if (list !! n) == "2" then let newList = setAt (f(list !! var)) (show((f(list !! pos1))*(f(list !! pos2)))) list in (doOperations newList (n+4) newList)

                 else if (list !! n) == "99" then doOperations [] 0 nyaList
                      else doOperations [] 0 ["fel"] 
               
--Simple help function to change the type of the input (String) to Int          
f :: String -> Int
f s = read s :: Int

