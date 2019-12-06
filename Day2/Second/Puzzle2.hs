import Functions
import System.IO
import Functions2

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       print $ runEverything (splitOn "," numbers)

doOperations :: [String] -> Int -> [String]-> [String]
doOperations [] _ nyaList= nyaList
doOperations list n nyaList= do
    let pos1 = f(list !! (n+1))
    let pos2 = f(list !! (n+2))
    let var  = f(list !! (n+3))
    if (n >= ((length list)-3)) then 
        doOperations [] n nyaList 
    
    else if (list !! n) == "1" then let newList = setAt (f(list !! var)) (show((f(list !! pos1))+(f(list !! pos2)))) list in (doOperations newList (n+4) newList)

            else if (list !! n) == "2" then let newList = setAt (f(list !! var)) (show((f(list !! pos1))*(f(list !! pos2)))) list in (doOperations newList (n+4) newList)

                 else if (list !! n) == "99" then doOperations [] 0 nyaList
                      else doOperations [] 0 ["fel"] 
               
            
f :: String -> Int
f s = read s :: Int






runEverything :: [String] -> [String]
runEverything list = do
       case ("19690720" == show(head $ tail (doOperations list 0 [] ))) of
          True -> [head $ tail (doOperations list 0 [] )]
          False -> do
            let listan1 = (setAt 2 "67" list) 
            let listan = (setAt 1 "12" listan1)
            (doOperations listan 0 [] )
          
        
