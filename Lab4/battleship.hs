import Test.QuickCheck
import Data.Maybe
import Data.List
import System.Random
import Data.Eq
import Data.IntMap
import qualified Data.IntMap as Map
import qualified Data.List as List


--"part" of a Board
data CellState = Hit | Missed | SneakyShip | SneakyWater
   deriving (Eq, Bounded, Enum)

instance Show CellState where
  show Hit           = "X"
  show Missed        = "O"
  show SneakyShip    = "."
  show SneakyWater   = "."

data Board = Board {rows :: [[CellState]]}
    deriving (Show, Eq)

type Pos = (Int,Int)
type RandomBoards = IntMap(FilePath)

--Example boards
stdBoard :: Board
stdBoard = Board
    [[ss,ss,sw,sw]
    ,[sw,sw,sw,sw]
    ,[sw,ss,ss,sw]
    ,[sw,sw,sw,sw]]
  where
    h  = Hit
    m  = Missed
    ss = SneakyShip
    sw = SneakyWater

stdBoard2 :: Board
stdBoard2 = Board
    [[ss,ss,ss,ss]
    ,[ss,ss,ss,ss]
    ,[ss,ss,ss,ss]
    ,[ss,ss,ss,ss]]
  where
    h  = Hit
    m  = Missed
    ss = SneakyShip
    sw = SneakyWater

--For quickCheck
genCell :: Gen (CellState)
genCell = frequency [(7, rSneakyWater), (3, rSneakyShip)]

--For quickCheck
rSneakyShip :: Gen (CellState)
rSneakyShip = elements [SneakyShip]

--For quickCheck
rSneakyWater :: Gen (CellState)
rSneakyWater = elements [SneakyWater]

instance Arbitrary Board where
  arbitrary =
    do rows <- vectorOf 4 (vectorOf 4 genCell)
       return (Board rows)


--Prints a visual representation of the board
printBoard :: Board -> IO()
printBoard board = mapM_ putStrLn (List.map makePrintable (rows board))

--Makes the board to a String to be printed
makePrintable :: [(CellState)] -> String
makePrintable []     = []
makePrintable (x:xs) = (show x) ++ makePrintable xs


getState :: Board -> Pos -> CellState
getState board (x,y) = ((rows board) !! x) !! y

--Updates the board after a player have guessed
updateBoard :: Board -> Pos -> Board
updateBoard board pos | getState board pos == SneakyShip
                        = updateBoard' board pos Hit
updateBoard board pos | getState board pos == SneakyWater
                        = updateBoard' board pos Missed
updateBoard board pos | otherwise = board

updateBoard' :: Board -> Pos -> CellState -> Board
updateBoard' board (x,y) cs = Board $ replaceState (rows board)
                                (x, (replaceState ((rows board)
                                !! x) (y, cs)))

--Tests updateBoard and updateBoard'
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

--Replace the value of a state with another.
replaceState :: [a] -> (Int,a) -> [a]
replaceState [] (pos,val) = []
replaceState xs (pos,val) | length xs <= pos = xs
replaceState xs (pos,val) = let (ys,zs) = splitAt pos xs in ys ++ [val]
                                   ++ (tail zs)


--Checks if the game is finished
gameFinished :: Board -> Bool
gameFinished board | length [boardToList board |
                     row <- [0..(length (rows board)) - 1],
                     cell <- [0..((length (rows board)) -1)],
                     getState board (row,cell) == SneakyShip] == 0 = True
gameFinished board | otherwise = False

--Helps the gameFinished method
boardToList :: Board -> [CellState]
boardToList board  = [rightCells | rows <- (rows board), rightCells <- rows]

--Checks so the original board and generated list contains same nbr of cells
prop_boardToList :: Board -> Bool
prop_boardToList board = length (boardToList board)
                         == sum [length ((rows board) !! a)
                         | a<-[0..(length(rows board)-1)]]

--Funkar inte fukkkking helelelelelleellelelell
--Checks so the original board and generated list contains same cells
{-prop_boardToList2 :: Board -> Bool
prop_boardToList2 board = and [(list !! l) == ((rows board) !! y) !! x
                          | l<-[0..(length list)-1], x<-[0..(length (rows board))-1],
                            y<-[0..(length ((rows board)!!0))-1] ]
         where
           list = boardToList board-}



--Generates a random int which is the key to the IntMap with boards.
randomKey :: IntMap  FilePath ->  StdGen -> IO Board
randomKey boardMap g = readBoard (boardMap!key)
         where
           (key,g') = randomR (1, (Map.size boardMap)) g


readBoard :: FilePath -> IO Board
readBoard file = do
  boardFile <- readFile file
  return (Board $ List.map makeBoard $ lines boardFile)
      where
        makeBoard :: String -> [CellState]
        makeBoard (x:xs) | x == 'S' = SneakyShip : (makeBoard xs)
        makeBoard (x:xs) | x == 'W' = SneakyWater : (makeBoard xs)
        makeBoard x      | otherwise = []



main :: IO ()
main = do
  let theMap = fromList ([(1,"b1.boa"),(2,"b2.boa"),(3,"b3.boa"),(4,"b4.boa")
                        ,(5,"b5.boa"),(6,"b6.boa"),(7,"b7.boa"),(8,"b8.boa"),
                        (9,"b9.boa"),(10,"b10.boa"),(11,"b11.boa"),
                        (12,"b12.boa")])
  g1 <- newStdGen
  g2 <- newStdGen
  board1 <- (randomKey theMap g1)
  board2 <- (randomKey theMap g2)
  gameLoop board1 board2 1 2

announceWinner :: String -> IO()
announceWinner winner =
   do
     putStrLn ("Congratulations " ++ winner ++ " won! \n Play again? y or n")
     yn <- getLine
     if yn == "y"
        then main
     else
        putStrLn "Hope to see you again!"

gameLoop :: Board -> Board -> Int -> Int -> IO ()
gameLoop board1 board2 player1 player2 | gameFinished board1 &&
                                         gameFinished board2 =
                                         announceWinner "both players"
                                       | gameFinished board1 =
                                         announceWinner "Player 1"
                                       | gameFinished board2 =
                                         announceWinner "Player 2"
                                       | otherwise =
  do
    putStrLn "Player 1 board: "
    printBoard board1
    putStrLn "Player 2 board: "
    printBoard board2
    board1 <- (gameTurn board1 player1)
    putStrLn "Player 1 board: "
    printBoard board1
    putStrLn "Player 2 board: "
    printBoard board2
    board2 <- (gameTurn board2 player2)
    gameLoop board1 board2 player1 player2


gameTurn :: Board -> Int -> IO Board
gameTurn board player = do
  row <- makeGuess board player "row"
  cell <- makeGuess board player "column"
  return (updateBoard board ((row-1), (cell -1)))


makeGuess :: Board -> Int -> String -> IO Int
makeGuess board player rc =
       do
         putStrLn $ "Player "++ (show player) ++ " guess the " ++ rc
         ans <- readLn
         if ((ans-1) < length (rows board) && (ans-1) >= 0)
            then do return ans
               else do
                 putStrLn "Not okay"
                 makeGuess board player rc
