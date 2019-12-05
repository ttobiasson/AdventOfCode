module Puzzle1 where 

import System.IO
import Data.List

main :: IO ()
main = do
   file <- openFile "puzzle1.txt" ReadMode
   contents <- hGetContents file

   let x =  (floor mass / 3) -2
   print x