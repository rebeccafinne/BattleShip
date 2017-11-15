module BlackJack where
import Cards
import RunGame

import Test.QuickCheck hiding (shuffle)
import System.Random

--Part A

{-What happens in the size-function?
Given an exisiting hand the funciton calculates the number of cards in the
hand.
First it checks if it is empty, in that case the function returns 0.
Otherwise the function adds one and calls the function again and so on until
the remaining hand is empty. It returns the number of laps the function
 did. -}


empty :: Hand
empty = Empty

--Checks if the value of the hand is more than 21,
--in that case uses the value 1 for aces instead
value :: Hand -> Integer
value hand | preValue hand > 21 = (preValue hand) - (10 * numberOfAces hand)
value hand = preValue hand

--Helpfunctions for value
--Calculates the value of the hand for aces having a value of 11
preValue :: Hand -> Integer
preValue Empty = 0
preValue (Add card hand) = valueCard card + (preValue hand)

valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric m) = m
valueRank _ = 10

valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
numberOfAces (Add card hand) = numberOfAces hand

gameOver :: Hand -> Bool
gameOver hand | value hand > 21 = True
gameOver hand = False

winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest = Bank
winner guest bank | gameOver bank = Guest
winner guest bank | value guest > value bank = Guest
winner guest bank = Bank

--Part B


(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty
(<+) Empty (Add c h) = (Add c h)
(<+) (Add c h) Empty = (Add c h)
(<+) (Add c h) hand =  Add c (h <+ hand)


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3


prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1 + size h2) == (size (h1 <+ h2))

fullDeck :: Hand
fullDeck = allInSuit Clubs  <+ allInSuit Hearts
  <+ allInSuit Diamonds  <+ allInSuit Spades



allInSuit :: Suit -> Hand
allInSuit s = (Add (Card Ace s)(Add (Card King s) (Add (Card Queen s)
              (Add (Card Jack s)(Add (Card (Numeric 10) s)
              (Add (Card (Numeric 9) s)(Add (Card (Numeric 8) s)
              (Add (Card (Numeric 7) s)(Add (Card (Numeric 6) s)
              (Add (Card (Numeric 5) s)(Add (Card (Numeric 4) s)
              (Add (Card (Numeric 3) s)
              (Add (Card (Numeric 2) s)Empty)))))))))))))

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty h = error "draw: The deck is empty."
draw (Add d deck) h = (deck, (Add d h))

playBank :: Hand -> Hand
playBank deck  = h'
  where (d', h') = playBank' deck Empty

playBank' :: Hand -> Hand -> (Hand, Hand)
playBank' deck hand | value hand < 16 = playBank' deck' hand'
                    | otherwise = (deck, hand)
      where (deck', hand') = draw deck hand

--Picks a random number, n, and moves the card that is located in the n:th
--place to a new hand.
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g hand |size hand > 0 = (Add card' (shuffle g1 hand'))
  where (card', hand') = removeCard hand Empty v1
        (v1, g1) = randomR (0, ((size hand) - 1)) g


--Removes the n:th card from the original deck and returns the card and
--the new deck without the card.
removeCard :: Hand -> Hand-> Integer -> (Card, Hand)
removeCard (Add c h) hand n | n > 0 = removeCard h (Add c hand) (n-1)
                            | otherwise = (c, (h <+ hand))



belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g deck = size deck == size (shuffle g deck)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

implementation = Interface
      { iEmpty    = empty
      , iFullDeck = fullDeck
      , iValue    = value
      , iGameOver = gameOver
      , iWinner   = winner
      , iDraw     = draw
      , iPlayBank = playBank
      , iShuffle  = shuffle
      }

main :: IO ()
main = runGame implementation
