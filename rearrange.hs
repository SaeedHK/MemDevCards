import System.IO

--melem :: [a] -> Int -> Maybe a
melem :: [a] -> Int -> a
--melem [] _ = Nothing
melem (x:xs) n
--   | n <= 0 = Nothing
   | n ==1 = x
   | otherwise = melem xs (n-1)

mreplace :: [a] -> Int -> a -> [a]
mreplace [] _ _ = []
mreplace (x:xs) n a
 | n <= 0 = []
 | n == 1 = [a] ++ xs
 | otherwise = [x] ++ mreplace xs (n-1) a



mpermutefun ::  Integral a => a -> a -> a
mpermutefun n m = 1 + mod (2*m+1) n


--mpermute :: [a] -> [Maybe a]
mpermute :: [a] -> [a]
mpermute x = let ml=length x in [ melem x (mpermutefun ml m) | m <- [1..ml] ]

main = do
  contents <- readFile "devCards.csv"
  let mlines = unlines . mpermute . lines $ contents
  writeFile "devCardsRearr.csv" mlines
