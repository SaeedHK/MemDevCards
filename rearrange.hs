
ml = length y


mzip :: [a] -> [b] -> [(a,b)]
mzip [] _ = []
mzip _ [] = []
mzip (x:xs) (y:ys) = [(x,y)] ++ mzip xs ys

mpermuteinit :: [(Int, a)] -> [(Int, a)]
mpermuteinit y = [ (k,elemm) | (n,elemm) <- y let k = (2*n+1)%ml]

mput :: [(Int, a)] -> [a]
mput y = foldl [1..ml]

mpermute :: [a] -> [a]
mpermute x = (mzip [1..] x)
y = mzip [1..] x
ynew =

main = do
  contents <- getContents
  mlines = lines contents
