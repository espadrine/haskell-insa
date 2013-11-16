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
chop n l = take n l : (if (length l) > n then chop n (drop n l) else [])

-- Each sudoku box, given a sudoku.
boxs :: Matrix a -> [Row a]
boxs l = concat $ map ((map concat) . cols . map (chop boxsize)) (chop boxsize l)

main = print (boxs minimal)

