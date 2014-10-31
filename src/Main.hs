type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . expand . many prune . choices

valid :: Grid -> Bool
valid g =
    all nodups (rows g) &&
    all nodups (cols g) &&
    all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs =
    map ungroup . ungroup .
    map cols .
    group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs:group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

choices :: Grid -> Matrix [Digit]
choices = map (map choice)

choice :: Digit -> [Digit]
choice d = if blank d then digits else return d

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 rows =
    [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
    where
        (rows1, row:rows2) = break (any smallest) rows
        (row1, cs:row2) = break smallest row
        smallest cs = length cs == n
        n = minimum (counts rows)

counts :: Matrix [Digit] -> [Int]
counts = filter (/= 1) . map length . concat

complete :: Matrix [Digit] -> Bool
complete = all (all single)

single :: [a] -> Bool
single [_] = True
single _ = False

safe :: Matrix [Digit] -> Bool
safe m =
    all ok (rows m) &&
    all ok (cols m) &&
    all ok (boxs m)
    where
        ok row = nodups [x | [x] <- row]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
    where yss = cp xss

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy rows . pruneBy cols . pruneBy boxs

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
    where fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove _ [x] = [x]
remove fixed xs = filter (`notElem` fixed) xs

many :: (Eq a) => (a -> a) -> a -> a
many f x =  if x == x' then x else many f x'
    where x' = f x

displayGrid :: Grid -> IO ()
displayGrid g =
    mapM_ putStrLn g'
    where
        g' = map (map replaceZeros) g
        replaceZeros = \d -> if blank d then ' ' else d

main :: IO ()
main = do
    let g = [
            "004005700",
            "000009400",
            "360000008",
            "720060000",
            "000402000",
            "000080093",
            "400000056",
            "005300000",
            "006100900"]
    displayGrid g
    displayGrid $ head $ solve g
