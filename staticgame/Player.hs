module Player (
    playCard,
    makeBid
)
where
{-
Write a report describing your design and strategy here.

-- The player makes a bid amount based on the strength of the trumps they have in their hand, 
-- and the strength of the rest (non-trumps) in their hand
-- If the selected trump's is Two, each trump >= Nine is expected to win 
-- If the selected trump's rank is Ace, each trump >= Eight is expected to win
-- If the selected trump's rank is otherwise, each trump > selected trump's rank is expected to win
-- For the rest of the cards, each card that has a rank Two, Three, or Four is guaranteed to lose
-- For the rest of the cards, each card that has rank >= Nine is expected to win

-- The Player will play a card depending on the current status of the bids and tricks
-- If the Player's number of tricks won is not equal to the bid amount, aggressive behaviour is engaged
-- If it is, the Player will adopt a passive approach
-- Aggressive behaviour is defined as identifying if it is possible to win the current trick with 
-- the player's cards in hand, and if possible, play the strongest card in an attempt to win
-- If it is not possible to win, play the weakest card possible
-- The strongest card would be the highest rank of the 
-- matching suit (if not starting) > trump suit > other suits
-- Passive behaviour is the opposite of aggressive behaviour, where the Player attemps to play the 
-- weakest card
-}

import OhTypes
import OhHell

-- | Play a card for the current trick.
-- If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard pId playerHand allPlayersBids trump tricksSoFar currentTrickCards = do
    let plannedBid = getPlannedBid pId allPlayersBids
    let tricksWon = getTricksWon trump pId tricksSoFar
    let metBidAmount = plannedBid == tricksWon
    -- Starting a trick
    if length currentTrickCards == 0
        then if (metBidAmount == True) 
            -- Aim to lose
            then passiveStart playerHand trump
            -- Aim to win 
            else aggressiveStart playerHand trump
        else if (metBidAmount == True) 
            then passive playerHand trump currentTrickCards
            else aggressive playerHand trump currentTrickCards

-- | Bid the number of cards you can win based on the trump card and your hand.
--   last player to bid must obey "hook rule":
--   sum of bids must not equal number of tricks available
makeBid :: BidFunc
-- Return number of bids you want to win. 
makeBid trump playerHand noOfPlayers bidsSoFar 
    -- If it's the last player bidding, respect the "hook rule"
    | length bidsSoFar == noOfPlayers - 1 = do
        let plannedBid = getTrumpBid trump (getTrumps trump playerHand) + getRestBid (getRest trump playerHand)
        let ok = hookRule (bidsSoFar ++ [plannedBid]) noOfPlayers (length playerHand)
        makeLastBid ok plannedBid
    | otherwise = getTrumpBid trump (getTrumps trump playerHand) + getRestBid (getRest trump playerHand)

-- ==========================   Defined functions 	==========================

-- Get the suit of a given card
getSuit :: Card -> Suit
getSuit (Card s _) = s 

-- Get the rank of a given card
getRank :: Card -> Rank
getRank (Card _ r) = r

-- Adapted from FIT2102 Lecture 8
-- Sorts a list of cards in ascending order (ignoring suit)
sort :: [Card] -> [Card]
sort [] = []
sort (pivot:rest) = below pivot rest ++ [pivot] ++ above pivot rest
    where
        below p = part (\x -> getRank x < getRank p)
        above p = part (\x -> getRank x >= getRank p)
        part test l = (sort . (filter test)) l

-- =============== Functions for playFunc ===============

-- Get the current leading suit of a trick
getTrickSuit :: Trick -> Suit 
getTrickSuit theTrick = getSuit $ fst $ last theTrick

-- Get a player's bid amount
getPlannedBid :: PlayerId -> [(PlayerId, Int)] -> Int
getPlannedBid pId bids = snd $ head $ filter (\x -> pId == fst x) bids

-- Get the count of tricks won by a given player
getTricksWon :: Card -> PlayerId -> [Trick] -> Int
getTricksWon trump pId tricksSoFar = length $ filter ((==) pId) $ map (winner (getSuit trump)) tricksSoFar 

-- Get all cards that match the trick's leading suit
getMatching :: [Card] -> Trick -> [Card]
getMatching playerHand currentTrick = filter (\x -> getSuit x == getTrickSuit currentTrick) playerHand

-- Gets the card that would win a trick
getWinningCard :: Card -> Trick -> Card
getWinningCard trump trick = fst $ head $ filter (\x -> snd x == winner (getSuit trump) trick) trick

-- Decides whether it is possible to win the current trick or not
canIWin :: Card -> Card -> Card -> [Card] -> [Card] -> [Card] -> Bool 
canIWin trump winning starting matching trumps rest
    -- Lead suit is the trump suit
    | getSuit trump == getSuit starting = if length trumps /= 0 
        then (getRank $ last $ trumps) > (getRank winning) 
        else False
    -- Lead suit is not the trump suit, but the winning card's suit is the trump suit
    | getSuit trump /= getSuit starting && getSuit trump == getSuit winning = if length matching /= 0
        then if length trumps /= 0
            -- Strongest trump can beat the winning card
            then (getRank $ last $ trumps) > (getRank winning)
            -- Cannot play trumps or matching suit, play other suit (will lose)
            else False
        -- Must play matching suit (will lose)
        else False
    -- Neither lead suit nor the winning card's suit is not the trump suit
    | otherwise = if length matching /= 0
        -- Strongest card in the matching suit can beat the winning card
        then (getRank $ last $ matching) > (getRank winning)
        else if length trumps /= 0
            -- Play the weakest trump (as it will beat all other cards)
            then True
            -- Cannot play trumps or matching suit 
            else (getRank $ last $ rest) > (getRank winning)

-- Choose a card to play
chooseCard :: Bool -> [Card] -> [Card] -> [Card] -> Card
chooseCard canWin matching trumps rest 
    -- Can win but not with matching suit or trump cards
    | canWin == True && length matching == 0 && length trumps == 0 = last rest  
    -- Can win but not with matching suit
    | canWin == True && length matching == 0 && length trumps /= 0 = last trumps 
    -- Can win with matching suit
    | canWin == True && length matching /= 0 = last matching 
    -- Cannot win, and cannot play a card in matching suit or non-trump suits
    | canWin == False && length matching == 0 && length rest == 0 = head trumps
    -- Cannot win, and cannot play a card in matching suit
    | canWin == False && length matching == 0 = head rest
    -- Cannot win, and can play a card in matching suit 
    | otherwise = head matching

-- Starting off a trick with aggressive behaviour
aggressiveStart :: [Card] -> Card -> Card
aggressiveStart playerHand trump = do 
    let trumps = sort $ getTrumps trump playerHand
    let rest = sort $ getRest trump playerHand
    -- No cards of the trump suit in hand!
    if length trumps == 0
        -- Play the strongest card not of the trump suit
        then last rest
        -- Play the strongest card of the trump suit
        else last trumps

-- Continuing a trick with aggressive behaviour
aggressive :: [Card] -> Card -> Trick -> Card
aggressive playerHand trump currentTrick = do
    let matching = sort $ getMatching playerHand currentTrick
    let trumps = sort $ getTrumps trump playerHand
    let rest = sort $ getRest trump playerHand
    -- let winningCard = getWinningCard trump currentTrick
    let canWin = canIWin trump (getWinningCard trump currentTrick) (fst $ last $ currentTrick) matching trumps rest
    chooseCard canWin matching trumps rest

-- Starting a trick with passive behaviour
passiveStart :: [Card] -> Card -> Card
passiveStart playerHand trump = do 
    let trumps = sort $ getTrumps trump playerHand
    let rest = sort $ getRest trump playerHand
    -- No cards of the trump suit in hand!
    if length trumps == 0
        -- Play the weakest card not of the trump suit
        then head rest
        -- Play the weakest card of the trump suit
        else head trumps

-- Continuing a trick with passive behaviour
passive :: [Card] -> Card -> Trick -> Card
passive playerHand trump currentTrick = do
    let matching = sort $ getMatching playerHand currentTrick
    let trumps = sort $ getTrumps trump playerHand
    let rest = sort $ getRest trump playerHand
    -- Cannot play a card that matches the lead suit
    if length matching == 0
        -- Cannot play a card that matches the trump suit
        then if length trumps == 0
            -- Play strongest (or weakest?) card that does not match either suit
            then last $ rest
            -- Play weakest card that matches the trump suit
            else head $ trumps
        -- Play weakest card that matches the lead suit
        else head $ matching 

-- =============== Functions for makeBid ===============

-- Last player's decision when making a bid that obeys the "hook rule"	
makeLastBid :: Bool -> Int -> Int
makeLastBid ok plannedBid
    -- If the intended bid is 0 and it would violate the hook rule, the player is forced to bid 1
    | ok == False && plannedBid == 0 = 1 
    -- Play conservatively
    | ok == False && plannedBid > 0 = plannedBid - 1
    -- The player can play their intended bid!
    | otherwise = plannedBid

-- Get all cards of the trump suit in a player's hand
getTrumps :: Card -> [Card] -> [Card]
getTrumps trump = filter (\x -> getSuit x == getSuit trump)

-- Get the expected number of tricks that can be won by using up a player's trump cards
getTrumpBid :: Card -> [Card] -> Int
getTrumpBid _ [] = 0
getTrumpBid trump trumpsInHand
    | getRank trump == Two = foldr (\x acc -> if (getRank x) < Nine then acc else 1 + acc) 0 trumpsInHand
    | getRank trump == Ace = foldr (\x acc -> if (getRank x) < Eight then acc else 1 + acc) 0 trumpsInHand
    | otherwise = foldr (\x acc -> if (getRank x == Two) then acc else if (getRank x == Ace) then 1 + acc else if (getRank x) < (getRank trump) then acc else 1 + acc) 0 trumpsInHand

-- Get all cards NOT of the trump suit in a player's hand
getRest :: Card -> [Card] -> [Card]
getRest trump = filter (\x -> getSuit x /= getSuit trump)

-- Get the expected number of tricks that can be won by using up all the cards that do not belong to the trump suit
getRestBid :: [Card] -> Int
getRestBid [] = 0
getRestBid restInHand = 
    foldr (\x acc -> if (getRank x) == Two || (getRank x) == Three || (getRank x) == Four 
        then acc else if (getRank x) < Nine then acc else 1 + acc) 0 restInHand