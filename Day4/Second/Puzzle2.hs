import Data.List
import Debug.Trace
main :: IO ()
main = print . length $ [x | x <- [387638..919123], (f $ show x) && (g $ show x) && (h $ show x)]

f :: String -> Bool
f [] = False
f [x] = True
f (x:y:xs) = x <= y && f (y:xs)

g :: String -> Bool
g [] = False
g [x] = True
g (x:y:xs) = x == y || g (y:xs)

h :: String -> Bool
h s = ((elem 3 . map length . group $ s) && (elem 2 . map length . group $ s)) || (elem 2 . map length . group $ s)

