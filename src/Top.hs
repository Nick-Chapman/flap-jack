
module Top (main) where

import System.Random (getStdRandom,randomR)
import Data.List (sortOn) --,sort,nub

newtype Card = Card Int -- 1..13, -1..-13
  deriving Show

newtype Hand = Hand [Card]
  deriving Show

data AceScoreChoice = Ace1 | Ace11

data AceHandling = AceHandling { pos:: AceScoreChoice, neg:: AceScoreChoice }

data HandSum = Bust | Accumulated Int -- Bust / -inf..25
  -- Bust orders below any accumulated score
  deriving (Eq,Ord,Show)

newtype HandScore = HandScore Int -- 10..0 (10 bad, 0 good)
  deriving (Eq,Ord,Show)

data CardAction = Twist | Stick

newtype Strat = Strat { runStrat :: [Card] -> CardAction }

newtype HandAction = HandAction { stickAfter :: Int } -- 0..26
  deriving Show

main :: IO ()
main = do
  putStrLn "*flap-jack*"
  hands <- randomHands 10000
  --mapM_ print hands
  -- run each strategy on all hands
  let results =
        [ (name, average (map (play strat) hands))
        | (name,strat) <- stratsToConsider
        ]
  -- print the results in ascending order; worse->better
  mapM_ print (reverse (sortOn snd results))
  print ("god-score", average (map godScore hands))

-- | Generate a list of n random hands.
randomHands :: Int -> IO [Hand]
randomHands n = do
  let all = [ Card i | i <- [1..13] ++ map negate [1..13] ]
  shuffles <- mapM shuffle (replicate n all)
  return $ map Hand shuffles

-- | A list of named-strategies for comparison.
stratsToConsider :: [(String,Strat)]
stratsToConsider =
  [("twist-"++show n, twistN n) | n <- [7] ] ++
  [("reach-"++show n, reachN n) | n <- [16..25] ]
--  [("reach-chances-"++show (a,b,c), reachChances (a,b,c)) | a <- [18..21], b <- [a..21], c <- [b..21] ]

-- | Twist a fixed number of cards (a really dumb strategy).
twistN :: Int -> Strat
twistN n = Strat (\seen -> if length seen >= n then Stick else Twist)

-- | Twist until we reach an accumulated goal: 16..25 (an ok simple strategy).
reachN :: Int -> Strat
reachN n = Strat $ \seen -> do
  if n < 16 then error "reachN, n<16, silly" else
    if n > 25 then error "reachN, n>25, silly" else do
      case sumTwisted seen of
        Bust -> Stick
        Accumulated acc -> if acc >= n then Stick else Twist

-- | Play a strategy for a given hand, and compute the score.
play :: Strat -> Hand -> HandScore
play strat hand = do
   let (Hand cards) = hand
   let HandAction{stickAfter} = playStrat strat hand
   let sum = sumTwisted (take stickAfter cards)
   let score = scoring sum
   score

-- | Play a strategy for a given hand, determining how many twists it makes.
playStrat :: Strat -> Hand -> HandAction
playStrat strat (Hand cards) =
  if length cards /= 26 then error "playStrat, #cards !=26" else
    loop 0
  where
    loop :: Int -> HandAction
    loop n =
      if n > 26 then error "playStrat,loop,n>26" else
        if n==26 then HandAction { stickAfter = 26 } else
          case runStrat strat (take n cards) of
            Stick -> HandAction {stickAfter = n}
            Twist -> loop (n+1)

-- | Best possible score for a hand
godScore :: Hand -> HandScore
godScore (Hand cards) = scoring (maximum [ sumTwisted (take n cards) | n <- [0..26] ])

-- | Compute the accumulated-sum/bust for a sequence of twisted cards.
sumTwisted :: [Card] -> HandSum
sumTwisted twisted = do
  maximum [sumTwistedAH ah twisted | ah <- allAceHandling]

-- | Compute the accumulated-sum/bust for a sequence of twisted cards (given specific ace-handling)
sumTwistedAH :: AceHandling -> [Card] -> HandSum
sumTwistedAH ah twisted = do
  loop ah 0 twisted
  where
    loop :: AceHandling -> Int -> [Card] -> HandSum
    loop ah acc = \case
      [] -> Accumulated acc
      card:cards -> do
        let acc' = acc + faceValue ah card
        if acc' > 25 then Bust else loop ah acc' cards

-- | All four ways of playing the two aces.
allAceHandling :: [AceHandling]
allAceHandling =
  [AceHandling{pos,neg} | pos <- [Ace1,Ace11], neg <- [Ace1,Ace11] ]

-- | Given a specific way of playing the aces, determine a card's face-value.
faceValue :: AceHandling -> Card -> Int
faceValue AceHandling{pos,neg} (Card n) =
  if
    | n==0      -> error "faceValue,n==0"
    | n==1      -> case pos of Ace1 -> 1; Ace11 -> 11
    | n==(-1)   -> case neg of Ace1 -> -1; Ace11 -> -11
    | n>10      -> 10
    | n<(-10)   -> -10
    | otherwise -> n

-- | Turn an accumulated-sum/bust into a score(0..10).
scoring :: HandSum -> HandScore
scoring = \case
  Bust -> HandScore 10
  Accumulated n ->
    if n>25 then error "scoring, n>25" else
      if n<16 then HandScore 10 else
        HandScore (25-n)

-- | Compute the average of a collection of scores.
average :: [HandScore] -> Float
average = \case
  [] -> error "average[]"
  xs -> do
    let n = sum [ i | HandScore i <- xs ]
    fromIntegral n / fromIntegral (length xs)

-- | Shuffle a list of items.
shuffle :: [a] -> IO [a]
shuffle = \case
  [] -> pure []
  xs -> do
    i <- randUpto (length xs)
    let (before,chosen:after) = splitAt i xs
    rest <- shuffle (before++after)
    return $ chosen:rest

-- | Return a random number (0,n-1).
randUpto :: Int -> IO Int
randUpto n =
  if n<=0 then error "randUpto, n<=0" else
    getStdRandom (randomR (0,n-1))


{-
-- Try more complicated strategy, based on ace-chances in seen cards
-- But does not achieve anything useful :(

-- | Twist until we reach goals a/b/c, depending on chanced-sum
reachChances :: (Int,Int,Int) -> Strat
reachChances (a,b,c) = Strat $ \seen -> do
  case chancedSum seen of
    Busted -> Stick
    AccNoChances acc -> if acc >= a then Stick else Twist
    AccOneChance acc -> if acc >= b then Stick else Twist
    AccTwoChances acc -> if acc >= c then Stick else Twist

data ChancedSum = Busted | AccNoChances Int | AccOneChance Int | AccTwoChances Int

chancedSum :: [Card] -> ChancedSum
chancedSum twisted =
  case nub (sort [n | ah <- allAceHandling, Accumulated n <- [sumTwistedAH ah twisted] ]) of
    [] -> Busted
    [x] -> AccNoChances x
    [_,x] -> AccOneChance x
    [_,_,x] -> AccTwoChances x
    _ -> error "chancedSum"
-}
