import Data.List hiding (transpose)

-- Major data types.
type Grid     = Matrix Value
type Matrix a = [Row a]
type Row a    = [a]
type Value    = Char

-- Basic definitions.
boxsize    :: Int
boxsize    = 3

values     :: [Value]
values     = ['1'..'9']

empty      :: Value -> Bool
empty      = (== '.')

single     :: [a] -> Bool
single [_] = True
single _   = False

-- Example grids.
easy :: Grid
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",
        "4..25...."]

gentle :: Grid
gentle = [".1.42...5",
          "..2.71.39",
          ".......4.",
          "2.71....6",
          "....4....",
          "6....74.3",
          ".7.......",
          "12.73.5..",
          "3...82.7."]

diabolical :: Grid
diabolical = [".9.7..86.",
              ".31..5.2.",
              "8.6......",
              "..7.5...6",
              "...3.7...",
              "5...1.7..",
              "......1.9",
              ".2.6..35.",
              ".54..8.7."]

unsolvable :: Grid
unsolvable = ["1..9.7..3",
              ".8.....7.",
              "..9...6..",
              "..72.94..",
              "41.....95",
              "..85.43..",
              "..3...7..",
              ".5.....4.",
              "2..8.6..9"]

minimal :: Grid
minimal = [".98......",
           "....7....",
           "....15...",
           "1........",
           "...2....9",
           "...9.6.82",
           ".......3.",
           "5.1......",
           "...4...2."]

-- Create an empty grid.
blank :: Grid
blank = replicate n (replicate n '.') where n = boxsize ^ 2

-- Some extraction functions.

-- Each sudoku row, given a sudoku.
rows :: Matrix a -> [Row a]
rows = id

-- Each sudoku column, given a sudoku.
cols :: Matrix a -> [Row a]
cols ([]:_) = []
--cols l = (map head l) : (cols (map tail l))
-- Or:
--cols l = [ map (!! n) l | n <- [0..(length l - 1)] ]
-- Or:
cols ((x:xs):xss) = (x:[h | (h:_) <- xss]) : cols (xs : [t | (_:t) <- xss])

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n l = take n l : chop n (drop n l)

-- Each sudoku box, given a sudoku.
boxs :: Matrix a -> [Row a]
boxs l = concat $ map ((map concat) . cols . map (chop boxsize)) (chop boxsize l)
-- Other implementation to highlight map / reduce (packing / unpacking):
--box l = map concat . concat (map cols (split . map split l))
--        -------------------            -----------------
--          unpacking                       packing

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

-- Create all possible grid: go through choices and collapse.

type Choice = [Value]

choices :: Matrix Char -> Matrix Choice
choices = map (map (\v -> if empty v then values else [v]))

-- Cartesian product: [[0,1],[2]] -> [[0,2],[1,2]].
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [(h:ys) | h <- xs, ys <- cp xss]

-- Expand all choices in a list of possible grids.
collapse :: Matrix Choice -> [Grid]
collapse = cp . map cp

-- check validity of a grid.

valid :: Grid -> Bool
valid g = (all nodups (rows g)) && (all nodups (cols g)) && (all nodups (boxs g))

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

fminimal =["6.4925378",
           "3.5817469",
           "7896.4521",
           "1527.3896",
           "863192745",
           "947586132",
           "491278653",
           "236451987",
           "578369214"]
main = print (solve fminimal)

