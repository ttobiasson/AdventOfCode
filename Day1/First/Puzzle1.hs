import System.IO

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       print $ foldl (\ acc x -> floor ((fromIntegral x) / 3) -2  + acc) 0 
       ( map read $ words numbers :: [Integer] )

