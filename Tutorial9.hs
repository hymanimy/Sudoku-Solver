-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

slice :: Int -> Int -> [a] -> [a]
slice a b xs = take (b-a+1) (drop a xs)

group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n xs = [slice i (i + n-1) xs | i <- [0, n .. length xs - 1]]

intersperse :: a -> [a] -> [a]
intersperse d = foldr (\ x -> (++) [d, x]) [d] 

showRow :: String -> String
showRow = concat . intersperse "|" . group

showGrid :: Matrix Digit -> [String]
showGrid = concat . intersperse ["-------------"] . group . map showRow

put :: Matrix Digit -> IO ()
put = putStr . unlines . showGrid 

showMat :: Matrix Digit -> String
showMat m = concat [[dot v | v <- r]
                           | r <- m]
    where dot chr | blank chr = '.'
                  | otherwise = chr

readMat :: String -> Matrix Digit
readMat m = [map dtb r | r <- groupBy 9 m]  
    where dtb '.' = ' ' 
          dtb  x  =  x 

choices :: Matrix Digit -> Matrix [Digit]
-- choices m = [[btnum ((m !! i) !! j)  | j <- [0..length m - 1]]
--                                      | i <- [0..length (head m) - 1]]
--         where btnum ' ' = "123456789"
--               btnum x   = [x]
choices m = [[btnum v | v <- r]
                      | r <- m]
        where btnum ' ' = "123456789"
              btnum x   = [x]

cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)

expand :: Matrix [Digit] -> [Matrix Digit]
expand = cp . map cp

prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == product (map (product . map length) m)

easySols :: Integer
easySols = fromIntegral (product (map (fromIntegral . product . map length) (choices easy)))

timereq :: Double
timereq = (fromIntegral easySols / 10^12) / (60 * 60 * 24 * 365.25)

mag :: Double
mag = timereq / (13.7 * 10^9)

-- time required for easySols to be computed would be 1.8 * 10^14 times the age of the universe

rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose 
-- boxs m = (map ungroup . ungroup) (map cols ((group . map group) m))
boxs = map ungroup . ungroup . map cols . group . map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat


distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs


valid :: Matrix Digit -> Bool
valid g = all distinct (rows g) && all distinct (cols g) && all distinct (boxs g)


-- This is not a viable solution as the computation time would exceed how long humans have left in the universe
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

the :: [Digit] -> Digit
the [d] = d

-- removes a single digit from a string, if the string is not equal to the digit
rm :: Eq a => a -> [a] -> [a]
rm dig xs | xs == [dig] = xs
          | otherwise   = filter (/= dig) xs

-- removes several digits from a string. 
remove :: Eq a => [a] -> [a] -> [a]
remove [] xs   = xs
remove (d:ds) xs = remove ds (rm d xs)

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow = helper [] 
    where 
        helper :: Row [Digit] -> Row [Digit] -> Row [Digit]
        helper row [] = row 
        helper pre ([d]:suf) = helper ((rm d pre) ++ [[d]]) (rm d suf)
        helper pre (d:suf) = helper (pre++[d]) suf 
        rm :: Digit -> Row [Digit] -> Row [Digit]
        rm d = map (filter (/=d))

-- Checking of identities
-- (rows . rows $ easy) == easy 
-- (cols . cols $ easy) == easy
-- (boxs . boxs $ easy) == easy

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f 

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows 

many :: Eq a => (a -> a) -> a -> a
many f m | f m == m  = m
         | otherwise = many f (f m) 

extract :: Matrix [Digit] -> Matrix Digit
extract m = [[ the v | v <- r ]
                     | r <- m ]

-- solves easy and medium
solve :: Matrix Digit -> Matrix Digit
solve = extract . many prune . choices 


failed :: Matrix [Digit] -> Bool
failed mat = or [or [ null v | v <- r ]
                             | r <- mat]

solved :: Matrix [Digit] -> Bool
solved mat = and [and [ length v == 1 | v <- r ]
                                      | r <- mat]
    



shortest = mini . map (mini . map length)
    where mini xs = if all (<=1) xs then 1 else minimum (filter (>1) xs)

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds]
    where (preMat, row:postMat) = break (any (\x -> length x == shortest mat)) mat 
          (preRow, ds:postRow)  = break (\x -> length x == shortest mat) row


search :: Matrix Digit -> [Matrix Digit]
search = map extract . helper . many prune . choices 
    where 
        helper :: Matrix [Digit] -> [Matrix [Digit]]
        helper mat | solved mat = [mat]
                   | failed mat = []
                   | otherwise  = concat(map (helper . many prune) (expand1 mat))

-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

