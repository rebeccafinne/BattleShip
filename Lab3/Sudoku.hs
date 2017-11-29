
import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List


-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
        Sudoku
          [ [j 3,j 6,j 5 ,j 9 ,j 7,j 1,j 2,j 4 ,j 8 ]
          , [j 3 ,j 5,j 6,j 7 ,j 2 ,j 4 ,j 1,j 8,j 9]
          , [j 7 ,j 1,j 9,j 2,j 5 ,j 4,j 7,j 9 ,j 6 ]
          , [j 2 ,j 5 ,j 3 ,j 4 ,j 1,j 3,j 6,j 2,j 8]
          , [j 4,j 4 ,j 8,j 5,j 9 ,j 2,j 3 ,j 6 ,j 9]
          , [j 2,j 7,j 5 ,j 4,j 6,j 3 ,j 6 ,j 8,j 9]
          , [j 3 ,j 6 ,j 5,j 3,j 4 ,j 8,j 9,j 2 ,j 9 ]
          , [j 8 ,j 8,j 3,j 8  ,j 3  ,j 2  ,j 1   ,j 6,j 6  ]
          , [j 2 ,j 5  ,j 7,j 6,j 9,j 8 ,j 9 ,j 4,j 3]
          ]
      where
        n = Nothing
        j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = and [isRow s, rightNumber s, isColumns s]

isRow :: Sudoku -> Bool
isRow s = length (rows s) == 9


rightNumber :: Sudoku -> Bool
rightNumber s = and (map isNum (rows s))


isNum :: [Maybe Int] -> Bool
isNum s | and ([isNothing x | x <- s]) = True
isNum s | maximum ([fromJust x | x <- filter isJust s])>9 = False
isNum s | minimum ([fromJust x | x <- filter isJust s])<0 = False
isNum s | otherwise = True

isColumns :: Sudoku -> Bool
isColumns s = foldr (+) 0 (map length (rows s)) == 81

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = and (map and ([map isJust x | x <- rows s]))

-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s =  mapM_   putStrLn (map makePrintable (rows s))


makePrintable :: [(Maybe Int)] -> String
makePrintable [] = []
makePrintable (x:xs) | isJust x == False =   (".") ++ (makePrintable xs)
makePrintable (x:xs) | otherwise =  (show (fromJust x))
                       ++ (makePrintable xs)

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
                       s <- readFile file
                       return (Sudoku $ map makeSudoku $ lines s)

                where
                  makeSudoku :: String -> [Maybe Int]
                  makeSudoku (x:xs) | x == '.' = Nothing: (makeSudoku xs)
                  makeSudoku (x:xs) | isHexDigit x = (Just (digitToInt x))
                                                   : (makeSudoku xs)
                  makeSudoku x      | otherwise = []



-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell =  frequency [(9, rNothing),
                   (1, rJust)]

rJust :: Gen (Maybe Int)
rJust = elements [Just 1, Just 2, Just 3, Just 4, Just 5,
                  Just 6, Just 7, Just 8, Just 9]

rNothing :: Gen (Maybe Int)
rNothing = elements [Nothing]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

-------------------------------------------------------------------------

-- * D1

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock block | (length $ nub (filter isJust block)) ==
                    length (filter isJust block) = True
isOkayBlock block | otherwise = False

-- * D2

blocks :: Sudoku -> [Block]
blocks sud = (createRows sud) ++ (createColums sud)  ++ (create3x3 sud)

createRows :: Sudoku -> [Block]
createRows sud = [row | row <- rows sud]

createColums :: Sudoku -> [Block]
createColums sud = [createColumn sud nr | nr <- [1..9]]

createColumn :: Sudoku -> Int -> Block
createColumn sud nr = concat [take 1 (drop (nr-1) row) | row <- (rows sud)]

create3x3 :: Sudoku -> [Block]
create3x3 sud =  [createSquare sud nrHor nrVer | nrHor <- [0, 3, 6],
                                                 nrVer <- [0,3,6]]

createSquare :: Sudoku -> Int -> Int -> Block
createSquare sud nrHor nrVer = concat [take 3 (drop (nrHor) row)
                              | row <- take 3 (drop nrVer (rows sud))]

prop_Sudoku_27 :: Sudoku -> Bool
prop_Sudoku_27 sud = (length (blocks sud)== 27) &&
                     (and [length block == 9 | block <- blocks sud])

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud = and [isOkayBlock block | block <- blocks sud]

-- * E1

type Pos = (Int,Int)

-- Goes though every row and cell to check if is blank and
-- add blank positions to a list.
blanks :: Sudoku -> [Pos]
blanks sud = [(row,cell) | row<-[0..8], cell<-[0..8],
                           ((rows sud)!!row)!!cell == Nothing]


prop_blanks :: Sudoku -> Bool
prop_blanks sud = and [isNothing $ (rows sud)!!x!!y | (x,y) <- blanks sud]
-- * E2

-- If the list is empty there is no value to change
-- If the length of the list is less or equal to the position nothing
-- is changed
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (pos,val) = []
(!!=) xs (pos,val) | length xs <= pos = xs
(!!=) xs (pos,val) = let (ys,zs) = splitAt pos xs in ys ++ [val]
                                   ++ (tail zs)


prop_replace :: [a] -> (Int, a) -> Bool
prop_replace xs (pos,val) = length xs == length ((!!=) xs (pos,val))

prop_replace' :: Eq a => [a] -> (Int, a) -> Bool
prop_replace' [] (pos,val) = ((!!=) [] (pos,val)) == []
prop_replace' xs (pos,val) =  val `elem` ((!!=) xs (pos,val))

-- * E3

-- Find the right position in the sudoku and switches the value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (row, cell) value = Sudoku $ (!!=) (rows sud)
                                (row, ((!!=) ((rows sud)
                                !! row) (cell, value)))

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update sud (row, cell) value = (rows (update sud (row', cell') value)
                                    !! row') !! cell' == value
    where row' = row `mod` 8
          cell' = cell `mod` 8

-- * E4

-- Finds all possible values for a certain position
candidates :: Sudoku -> Pos -> [Int]
candidates sud (row,cell) = [ x | x <- [1..9], isOkay
                                (update sud (row,cell) (Just x))]


prop_candidates ::Sudoku -> Pos -> Bool
prop_candidates sud (row, cell) = and [ isOkay (update sud (row',cell')
                                    (Just x))  | x <- candidates sud
                                    (row',cell') ]
      where
        row' = row `mod` 8
        cell' = cell `mod` 8

-- * F1

-- Checks if the sudoku is okay before sending it to the help funciton
solve :: Sudoku -> Maybe Sudoku
solve sud | isOkay sud && isSudoku sud == True = solve' sud
          | otherwise = Nothing


-- If the sudoku is filled but not okay Nothing is returned
-- If there is no canditades for the position Nothing is returned
-- Otherwise recursivly try all candidates for the first blank position
solve' ::Sudoku -> Maybe Sudoku
solve' sud | (isFilled sud) && isOkay sud = Just sud
solve' sud | (isFilled sud) = Nothing
solve' sud | (candidates sud ((blanks sud)!!0) == []) = Nothing
solve' sud = listToMaybe  [fromJust (solve' (update sud ((blanks sud)!!0)
                          (Just value)))| value <- (candidates sud
                          ((blanks sud)!!0)), solve'
                          (update sud ((blanks sud)!!0)
                          (Just value)) /= Nothing]

-- * F2


readAndSolve :: FilePath -> IO ()
readAndSolve file = do sud <- readSudoku file
                       if solve sud == Nothing
                         then putStrLn "(no solution)"
                         else
                           printSudoku (fromJust (solve sud))


-- * F3

-- Checks if the filled numbers in sud2 is the same as in sud1
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 | not (isFilled sud1) || not (isSudoku sud1) ||
                         not (isOkay sud1) = False
isSolutionOf sud1 sud2 = and [(((rows sud1)!!x)!!y)==(((rows sud2)!!x)!!y)
                         | x<-[0..8], y<-[0..8],
                         (((rows sud2)!!x)!!y) /= Nothing]


-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isJust (solve sud) ==>
                          isSolutionOf (fromJust (solve sud)) sud

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 15 } prop
