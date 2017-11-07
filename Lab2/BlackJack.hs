module BlackJack where
import Cards
import RunGame

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
preValue (Add card hand) = valueCard card + (value hand)

valueRank :: Rank -> Integer
valueRank King = 10
valueRank Queen = 10
valueRank Jack = 10
valueRank Ace = 11
valueRank (Numeric m) = m

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























