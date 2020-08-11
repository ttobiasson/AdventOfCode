import System.IO
--https://adventofcode.com/2019/day/1

main :: IO ()
main = openFile "inputDay1.txt" ReadMode >>=  --Open the file containing the input             
       hGetContents >>= \numbers ->
       print . 
       sum . 
       recs $ (map read $ words numbers :: [Integer])

recs :: [Integer] -> [Integer]
recs [] = []
recs (x:xs) = let newNth = (recs' x) - x in  --Recursively calculate the fuel cost for the added fuel
                  newNth : recs xs           --and add it to the previous calculations

recs' :: Integer -> Integer
recs' x = if x <= 0 then 0 else                                         --Mass that requires negative 
                                                                        --fuel gets treated as 
                                                                        --requiring 0 fuel
    let c = map (\x -> (floor ( (fromIntegral x) / 3) ) -2 ) [x] in --Do the same calculation as part one
    x + recs' (head c)                                              --But here we do it until we reach 0 or less.
