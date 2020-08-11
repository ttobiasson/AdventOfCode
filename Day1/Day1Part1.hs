import System.IO
--https://adventofcode.com/2019/day/1

main :: IO ()
main = openFile "inputDay1.txt" ReadMode >>=    --Open the file containing the input             
       hGetContents >>= \numbers ->
       print .                                  --Print the result of the calculations
       foldl (\ acc x -> floor ((fromIntegral x) / 3) -2  + acc) 0 --Do a simple calculation on one
                                                                   --number at a time, save the result 
                                                                   --and add it to the result of the
                                                                   --remaining numbers in the list
       $ ( map read $ words numbers :: [Integer] ) --Change the type of the numbers to Integer

