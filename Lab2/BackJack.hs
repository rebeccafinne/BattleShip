module BlackJack where
import Cards
import RunGame

empty :: Hand

value :: Hand -> Integer
--Helpfunctions for value
valueRank :: Rank -> Integer
valueCard :: Card -> Integer
numberOfAces :: Hand -> Integer

gameOver :: Hand -> Bool

winner :: Hand -> Hand -> Player
