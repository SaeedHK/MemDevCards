import System.IO

melem :: [a] -> Int -> a
melem (x:xs) n
   | n ==1 = x
   | otherwise = melem xs (n-1)

mreplace :: [a] -> Int -> a -> [a]
mreplace [] _ _ = []
mreplace (x:xs) n a
 | n <= 0 = []
 | n == 1 = [a] ++ xs
 | otherwise = [x] ++ mreplace xs (n-1) a

mpermutefun ::  Integral a => a -> a -> a
mpermutefun n m = 1 + mod (8831*m) n
-- 8831 is a prime number that is big enough for our usage

mpermute :: [a] -> [a]
mpermute x = let ml=length x in [ melem x (mpermutefun ml m) | m <- [1..ml] ]






main = do
  contents <- readFile "devCards.csv"
  let mlines = unlines . mpermute . lines $ contents
  writeFile "devCardsRearr.csv" mlines
