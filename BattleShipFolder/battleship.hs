import Test.QuickCheck
import Data.Maybe
import Data.List
import System.Random
import Data.Eq


-- | "part" of a Board
data CellState = Hit | Missed | SneakyShip | SneakyWater
   deriving (Eq, Bounded, Enum)

instance Show CellState where
  show Hit           = "X"
  show Missed        = "O"
  show SneakyShip    = "."
  show SneakyWater   = "."

data Board = Board {rows :: [[CellState]], size::Int}
    deriving (Show, Eq)

type Pos = (Int,Int)

-- | Example boards
stdBoard :: Board
stdBoard = Board
    {rows=[[ss,ss,sw,sw]
    ,[sw,sw,sw,sw]
    ,[sw,ss,ss,sw]
    ,[sw,sw,sw,sw]],
    Main.size = 4}
  where
    h  = Hit
    m  = Missed
    ss = SneakyShip
    sw = SneakyWater

stdBoard2 :: Board
stdBoard2 = Board {
    rows = [[ss,ss,ss,ss]
    ,[ss,ss,ss,ss]
    ,[ss,ss,ss,ss]
    ,[ss,ss,ss,ss]],
    Main.size = 4}
  where
    h  = Hit
    m  = Missed
    ss = SneakyShip
    sw = SneakyWater

stdBoard3 :: Board
stdBoard3 = Board {
      rows = [[sw,sw,sw,sw]
      ,[sw,sw,sw,sw]
      ,[sw,sw,sw,sw]
      ,[sw,sw,sw,sw]],
      Main.size = 4}
    where
      h  = Hit
      m  = Missed
      ss = SneakyShip
      sw = SneakyWater

-- | For quickCheck
genCell :: Gen (CellState)
genCell = frequency [(7, rSneakyWater), (3, rSneakyShip)]

-- | For quickCheck
rSneakyShip :: Gen (CellState)
rSneakyShip = elements [SneakyShip]

-- | For quickCheck
rSneakyWater :: Gen (CellState)
rSneakyWater = elements [SneakyWater]

genSize :: Gen (Int)
genSize = elements [4..10]

instance Arbitrary Board where
  arbitrary =
    do
      bsize <- genSize
      rows <- vectorOf bsize (vectorOf bsize genCell)
      return (Board rows bsize)


-- | Prints a visual representation of the board
printBoard :: Board -> IO()
printBoard board = mapM_ putStrLn (map makePrintable (rows board))

-- | Makes the board to a String to be printed
makePrintable :: [CellState] -> String
makePrintable []     = []
makePrintable (x:xs) = (show x) ++ makePrintable xs


getState :: Board -> Pos -> CellState
getState board (x,y) = ((rows board) !! x) !! y

-- | Updates the board after a player have guessed
updateBoard :: Board -> Pos -> Board
updateBoard board pos | getState board pos == SneakyShip
                        = updateBoard' board pos Hit
updateBoard board pos | getState board pos == SneakyWater
                        = updateBoard' board pos Missed
updateBoard board pos | otherwise = board

updateBoard' :: Board -> Pos -> CellState -> Board
updateBoard' board (x,y) cs = Board  {rows = replaceState (rows board)
                                (x, (replaceState ((rows board)
                                !! x) (y, cs))), Main.size = Main.size board}

-- | Tests updateBoard and updateBoard'
prop_updateBoard :: Board -> Pos -> Bool
prop_updateBoard board (x,y) | getState board (x',y') == SneakyShip
                               = updateBoard' board (x',y') Hit
                               == updateBoard board (x',y')
                             | getState board (x',y') == SneakyWater
                               = updateBoard' board (x',y') Missed
                               == updateBoard board (x',y')
                             | otherwise = board == updateBoard board (x',y')
        where
          x' = x `mod` 3
          y' = y `mod` 3

-- | Replace the value of a state with another.
replaceState :: [a] -> (Int,a) -> [a]
replaceState [] (pos,val) = []
replaceState xs (pos,val) | length xs <= pos = xs
replaceState xs (pos,val) = let (ys,zs) = splitAt pos xs
                            in ys ++ [val] ++ (tail zs)


-- | Checks if the game is finished
gameFinished :: Board -> Bool
gameFinished board = not (any (== SneakyShip) (boardToList board))

prop_gameFinished :: Board -> Bool
prop_gameFinished b | gameFinished b == True
                      = SneakyShip `notElem` (boardToList b)
prop_gameFinished b | otherwise = SneakyShip `elem` (boardToList b)


--Helps the gameFinished method
boardToList :: Board -> [CellState]
boardToList board  = [rightCells | rows <- (rows board), rightCells <- rows]


-- | Checks so original board and generated list contains same nbr of cells
prop_boardToList :: Board -> Bool
prop_boardToList board = length (boardToList board)
                         == sum [length ((rows board) !! a)
                         | a<-[0..(length(rows board)-1)]]


-- | Checks so the original board and generated list contains same cells
prop_boardToList2 :: Board -> Bool
prop_boardToList2 board = list == [((rows board) !! x) !! y
                          | x<-[0..(length (rows board))-1],
                            y<-[0..(length ((rows board)!!0))-1]]
         where
           list = boardToList board

-- | Creates a random game board
createBoard :: Int -> StdGen -> Board
createBoard bSize g = insertShip (createWaterBoard bSize) g

-- | Inserts a ship on a random position on a board
insertShip :: Board -> StdGen -> Board
insertShip board g = updateBoard' board pos SneakyShip
                      where
                        pos = (getRandomPos g board)


getRandomPos :: StdGen -> Board -> Pos
getRandomPos g board = (row,cell)
     where
       (row,g1) = randomR (0,(Main.size board)) g
       (cell,g2) = randomR (0,(Main.size board)-1) g1


getRow :: Pos -> Int
getRow (row,cell) = row

getCell :: Pos -> Int
getCell (row, cell) = cell

-- | Creates a game board only consisting of water
createWaterBoard :: Int -> Board
createWaterBoard bSize = Board {rows = (replicate bSize
                    (replicate bSize SneakyWater)), Main.size = bSize}

-- | Takes user input for size of board
chooseBoardSize :: IO Int
chooseBoardSize = do
  bSize <- readLn
  if ((bSize<4) || (bSize>10))
     then do
       putStrLn "The size has to be between 4 and 10, please try again"
       chooseBoardSize
     else
       return bSize

main :: IO ()
main = do
  putStrLn "Choose a board size between 4 and 10"
  s <- chooseBoardSize
  g1 <- newStdGen
  g2 <- newStdGen
  let board1 = createBoard s g1
  let board2 = createBoard s g2
  putStrLn "Player 1, enter your name"
  p1 <- getLine
  putStrLn "Player 2, enter your name"
  p2 <- getLine
  gameLoop board1 board2 p1 p2


announceWinner :: String -> IO()
announceWinner winner =
   do
     putStrLn ("Congratulations " ++ winner ++ " won! \n Play again? y or n")
     yn <- getLine
     if yn == "y"
        then main
     else
        putStrLn "Hope to see you again!"

gameLoop :: Board -> Board -> String -> String -> IO ()
gameLoop b1 b2 p1 p2 | gameFinished b1 && gameFinished b2 =
                       announceWinner "both players"
                     | gameFinished b1 = announceWinner p1
                     | gameFinished b2 = announceWinner p2
                     | otherwise =
  do
    putStrLn (p1 ++ "\'s board: ")
    printBoard b1
    putStrLn (p2 ++ "\'s board: ")
    printBoard b2
    b1 <- (gameTurn b1 p1)
    putStrLn (p1 ++ "\'s board: ")
    printBoard b1
    putStrLn (p2 ++ "\'s board: ")
    printBoard b2
    b2 <- (gameTurn b2 p2)
    gameLoop b1 b2 p1 p2

-- | Make sure both players can make a guess
gameTurn :: Board -> String -> IO Board
gameTurn board player = do
  row <- makeGuess board player "row"
  cell <- makeGuess board player "column"
  return (updateBoard board ((row-1), (cell -1)))

-- | The player makes a guess, have to redo guess if
-- the number is outside the board
makeGuess :: Board -> String -> String -> IO Int
makeGuess board player rc =
       do
         putStrLn $ "Player " ++ player ++ " guess the " ++ rc
         ans <- readLn
         if ((ans-1) < length (rows board) && (ans-1) >= 0)
            then do return ans
               else do
                 putStrLn "Not okay"
                 makeGuess board player rc
