import System.IO

main :: IO ()
main = openFile "puzzle1.txt" ReadMode >>= 
       hGetContents >>= \numbers ->
       print $ sum $ recs (map read $ words numbers :: [Integer])

recs :: [Integer] -> [Integer]
recs [] = []
recs (x:xs) = let newNth = (recs' x) - x in
                  newNth : recs xs

recs' :: Integer -> Integer
recs' x = if x <= 0 then 0 else 
    let c = map (\x -> (floor ( (fromIntegral x) / 3) ) -2 ) [x] in
    x + recs' (head c)