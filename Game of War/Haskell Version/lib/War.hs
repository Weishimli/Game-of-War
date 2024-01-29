module War (deal) where

import Data.List

{--
Function stub(s) with type signatures for you to fill in are given below. 
Feel free to add as many additional helper functions as you want. 

The tests for these functions can be found in src/TestSuite.hs. 
You are encouraged to add your own tests in addition to those provided.

Run the tester by executing 'cabal test' from the war directory 
(the one containing war.cabal)
--}
    
deal :: [Int] -> [Int]
deal shuf = 
  let (player1, player2) = shuffle shuf
  in
    play_game player1 player2 []

               
shuffle :: [Int] -> ([Int], [Int]) --method that takes in a list and returns the a tuple of list for player1 and player 2
shuffle cards = shuffle1 cards [] [] 1 
  where 
    shuffle1 :: [Int] -> [Int] -> [Int] -> Int -> ([Int], [Int])
    shuffle1 [] player1 player2 _ = (player1,player2)    -- stop shuffling and return tuple when deck is empty
    shuffle1 (card:rest) player1 player2 player
      | player == 1 = shuffle1 rest (card : player1) player2 2 
      | player == 2 = shuffle1 rest player1 (card : player2) 1 


-- start of game between 2 players until there is a winner or tied
play_game :: [Int] -> [Int] -> [Int] -> [Int] 
play_game [] [] tied  = tied          
play_game [] p2 tied = p2 ++ tied
play_game p1 [] tied = p1 ++ tied

play_game (p1card:p1rest) (p2card:p2rest) tied = 
  let cards = 
        if null tied then   -- if war continue and there is no more cards left for either player, return tied pile
          sortWithPriority [p1card, p2card]
        else
          sortWithPriority ([p1card, p2card] ++ tied )
  
  in 
    if p2card /= 1 && (p1card > p2card || p1card == 1) then -- condition for when player1 is winner
      play_game (p1rest ++ cards) p2rest []
    else if p1card < p2card || p2card == 1 && p1card /= 1 then -- condition for when player2 is a winner
      play_game p1rest (p2rest ++ cards) []
    else if not (null p1rest) && not ( null p2rest) then -- condition for when there is a tie and it goes to war
        let (p1card1:p1rest1) = p1rest  --facedown cards
            (p2card1:p2rest1) = p2rest
        in
            play_game p1rest1 p2rest1 (sortWithPriority(cards ++ [p1card1, p2card1]) )
    else 
      play_game p1rest p2rest cards


compareWithPriority :: (Ord a, Num a) => a -> a -> Ordering  --Creating my own sorting criteria in descending order with 1 as priority
compareWithPriority x y
  | x == 1 && y == 1 = EQ
  | x == 1 = LT
  | y == 1 = GT
  | otherwise = compare y x

sortWithPriority :: (Ord a, Num a) => [a] -> [a]
sortWithPriority xs = sortBy compareWithPriority xs --sorting based on my custom criteria