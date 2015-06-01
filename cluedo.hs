{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}

import Data.Ord
import Data.List
import Data.List.Split
import Data.Maybe (catMaybes)
import Data.Function (on)
import System.Random
import Control.Monad.State
import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Show.Pretty as Pr

---------------------------------------------------------------------
-- | define the cards in the deck
-------------------------------------------------------------------------------
data Card = MS | CM | MW | RG | MP | PP | Ck | Dg | Lp | Rp | Sp | Kn | Bl | Cn | Dr | Br | Lb | Lg | Hl | Sy deriving (Eq, Enum, Show, Ord)
type Suggestion = (Card, Card, Card) -- constrained to be (s,w,p)
type Hand = [Card]
type Deck = [Card]
type Shuffled = [Card]
type PlayerID = Int
type SuggestionBy = (PlayerID, Suggestion) -- actual suggestions are made by players

suspectCards = [MS .. PP]
weaponCards = [Ck .. Sp]
placeCards = [Kn .. Sy]
allCards = [MS .. Sy] -- | full deck
suggestions = [(s,w,p) | s <- suspectCards, w <- weaponCards, p <- placeCards]
-------------------------------------------------------------------------------

-- | generic functions
-------------------------------------------------------------------------------
shuffle :: [a] -> [a]
shuffle as = snd $ unzip slist
    where
        rlist = randomRs (0, length as) (mkStdGen 42)
        slist = sortBy (comparing fst) $ zip rlist as

readAInt :: IO Int
readAInt = readLn

-- StackOverflow solution by cchambers 
-- http://stackoverflow.com/questions/29924256
keys :: Ord k => [k] -> IndexedTraversal' k (Map.Map k a) a
keys ks f m = go ks
  where
    go []     = pure m
    go (i:is) =
      case Map.lookup i m of
        Just a  -> Map.insert i <$> indexed f i a <*> go is
        Nothing -> go is

-------------------------------------------------------------------------------
-- | typed functions / equations
shuffleDeck :: Deck -> Shuffled
shuffleDeck d = shuffle d

murderCards :: Shuffled -> Suggestion
murderCards s = head $ [(ss,sw,sp) | ss <- s, sw <- s, sp <- s, ss `elem` suspectCards, sw `elem` weaponCards, sp `elem` placeCards]

removeMurderCards :: Suggestion -> Shuffled -> Shuffled
removeMurderCards (sc,wc,pc) s = filter (/= sc) $ filter (/= wc) $ filter (/= pc) s

deal :: Shuffled -> Int -> [Hand]
deal deck n = map (map snd) $ groupBy ((==) `on` fst) $ sortBy (comparing fst) ts
    where
        ts = zip (cycle [1..n]) deck

filteredIndex :: PlayerID -> Int -> [PlayerID]
filteredIndex    pid         n   =  filter (/= pid) $ [1..n]

testSuggestion :: Suggestion -> Hand -> Bool
testSuggestion (s,w,p) h = or bools
    where
        asList = [s,w,p]
        bools = map (flip elem asList) h

evalSuggestion :: Suggestion -> [Hand] -> [Bool]
evalSuggestion _ [] = []
evalSuggestion s (h:hs) = testSuggestion s h : evalSuggestion s hs

makeSuggestion :: [Hand] -> Suggestion -> SuggestionResult
makeSuggestion hs s = (s, match, passes)
  where
    results = zip (evalSuggestion s hs) [1..(length hs)]
    passers = takeWhile (not . fst) results -- only players before the match
    passes = map snd passers  -- gets IDs of players who passed
    match  = lookup True results

-- core information used for deduction and tracking
-------------------------------------------------------------------------------
type CardSet = Set.Set Card
type SuggestionSet = Set.Set Suggestion
data Ownership = Unknown | Noone | ID PlayerID deriving (Show, Eq)

type CardFacts         = Map.Map Card Ownership
type PlayerCards       = Map.Map PlayerID CardSet
type PlayerCards'      = Map.Map PlayerID CardSet
type PlayerPassed      = Map.Map PlayerID SuggestionSet
type PlayerSuggestions = Map.Map PlayerID SuggestionSet
type PlayerMatched     = Map.Map PlayerID SuggestionSet
type SuggestionResult  = (Suggestion, Maybe PlayerID, [PlayerID])

initCardFacts :: CardFacts
initCardFacts = Map.fromList (zip allCards (repeat Unknown))

initCardSet :: CardSet
initCardSet = Set.empty

initSuggestionSet :: SuggestionSet
initSuggestionSet  = Set.empty

initMapOfCardSets :: Int -> Map.Map PlayerID CardSet
initMapOfCardSets n = Map.fromList $ zip [1..n] $ repeat initCardSet

initMapOfSuggestions :: Int -> Map.Map PlayerID SuggestionSet
initMapOfSuggestions n = Map.fromList $ zip [1..n] $ repeat initSuggestionSet

data KB = KB { _cf  :: CardFacts
             , _pc  :: PlayerCards        -- Map Player -> Set of Cards held
             , _pc' :: PlayerCards'       -- Map Player -> Set of Cards _not_ held
             , _ps  :: PlayerSuggestions  -- Map Player -> Set of Suggestions Made
             , _psc :: PlayerSuggestions  -- Map Player -> Set of Suggestion candidates
             , _pm  :: PlayerMatched      -- Map Player -> Set of Suggestions Matched
             , _pm' :: PlayerPassed       -- Map Player -> Set of Suggestions Passed
             , _count :: Int
             , _turn :: Int
             } deriving (Show, Eq)

initKB :: Int -> KB
initKB    n   = KB { _cf  = initCardFacts
                  , _pc  = initMapOfCardSets n
                  , _pc' = initMapOfCardSets n
                  , _ps  = initMapOfSuggestions n
                  , _psc = initMapOfSuggestions n
                  , _pm  = initMapOfSuggestions n
                  , _pm' = initMapOfSuggestions n
                  , _count = n
                  , _turn = 1
                  }

makeLenses ''KB

ppKB :: KB -> IO ()
ppKB    kb =  putStrLn $ Pr.ppShow kb 

addCardFact :: Card -> Ownership -> State KB ()
addCardFact    c       o         =  cf.at c .= Just o

addCardFacts :: CardFacts        -> State KB ()
addCardFacts    cfs              =  cf %= (Map.union cfs)

-- any individual match can yield a Card IFF two cards are not held
analyseOneMatch :: CardSet -> CardSet -> Suggestion -> Maybe Card
analyseOneMatch    cs         cs'       (s,w,p) 
    | noNewInfo          = Nothing   -- if we already have any one we can't deduce anything
    | length diff == 1   = Just (head diff)  -- if DON'T have two, we can deduce shown card
    | otherwise          = Nothing
    where
        suggSet   = Set.fromList [s,w,p]
        noNewInfo = not $ Set.null $ Set.intersection cs suggSet 
        diff      = Set.toList $ Set.difference cs' suggSet

-- function to analyse one players matched suggestions to see if something can be learnt
deduceFromMatches :: CardSet -> CardSet -> SuggestionSet  -> [Card]  
deduceFromMatches    doHave     dontHave   matches        =  deductions
  where
    deductions = catMaybes $ Set.toList $ Set.map (analyseOneMatch doHave dontHave) matches

deduceForPlayer :: PlayerID -> KB -> [Card]
deduceForPlayer    pid         kb =  deduceFromMatches (kb^.pc.ix pid) (kb^.pc'.ix pid) (kb^.pm.ix pid)

addPassers :: [PlayerID] -> Suggestion -> State KB ()     -- passers definitely don't have any of the cards in suggestion
addPassers    is            (s,w,p)    =  do 
  pm' . keys is %= Set.insert (s,w,p)                     -- add the suggestion to their passes list
  pc' . keys is %= Set.union (Set.fromList [s,w,p])       -- add the cards to their "cards not held" list

cardOwnerIdentified :: PlayerID -> Maybe Card -> State KB ()
cardOwnerIdentified    _           Nothing    = pc . ix 1 %= Set.union (Set.empty :: CardSet)
cardOwnerIdentified    pid         (Just c)   = do
  mykb <- get
  pc . ix pid %= Set.insert c                                   -- this player has the card
  pc' . keys (filteredIndex pid (mykb^.count))  %= Set.insert c -- ergo, other players don't

addMatch :: PlayerID -> Suggestion -> State KB ()
addMatch    pid         s          =  do
  mykb <- get
  let c = analyseOneMatch (mykb^.pc.ix pid) (mykb^.pc'.ix pid) s     -- if Just c then cardOwnerIdentified m c 
  cardOwnerIdentified pid c   -- 
  pm . ix pid %= Set.insert s -- add to list of suggestions matched

learnFromSuggestion :: SuggestionResult -> State KB ()    -- how can i remove the pattern match here?
learnFromSuggestion   (s, Nothing, ps)  = do
  addPassers ps s
learnFromSuggestion   (s, Just m, ps)   = do
  addPassers ps s
  addMatch m s

solved :: KB -> Maybe Suggestion       -- should be an Either since there is distinct error condition
solved    kb =  case swp of
            []        -> Nothing       -- no solution is found
            (x:[])    -> Just x        -- single solution is found
            otherwise -> Nothing       -- multiple solutions found, algo is broken, see note on signature  
  where
    cfs = Map.toList $ kb^.cf
    swp = [ (s,w,p) | (s, Noone) <- cfs, (w, Noone) <- cfs, (p, Noone) <- cfs, elem s suspectCards, elem w weaponCards, elem p placeCards]

-------------------------------------------------------------------------------
-- temporary function for testing
learnFromSuggestions :: [SuggestionResult] -> State KB ()
learnFromSuggestions    srs                =  mapM_ learnFromSuggestion srs

{-
playRound :: KB -> KB
playRound k = newK
  where
    suggester = k^.turn `mod` k^.count
    newK = k^.turn +~ 1
-}


-- | Globals 
-------------------------------------------------------------------------------
d = allCards
s = shuffleDeck d
kb' = initKB 6
mc' = murderCards s
rc' = removeMurderCards mc' s
hs' = deal rc' 6
fs' = head suggestions
sr' = makeSuggestion hs' fs'
srs = map (makeSuggestion hs') suggestions
final = execState (learnFromSuggestions srs) kb'

main = do
    putStrLn "How many players?"
    n <- readAInt -- number of players
    let ps = [1..n] :: [PlayerID]
    let kb = initKB n
    let mc = murderCards s
    let remainingCards = removeMurderCards mc s
    let hs = deal remainingCards n
{-
 now let's have each player pick a suggestion which will be tested against the (hands - suggester)
 players pick suggestions at random but only from suggestions they haven't made
 it would be ideal if the suggestions were in some way strategic - two cards they have and two cards 
 they don't for example
   however the savvy player would use cards that they know are in the last of the line from them
   so as to avoid revealing what they have themselves?

 so something like this
  runState () 
-}
    let firstS = head suggestions
    let sResult = makeSuggestion hs firstS    -- should be hs-without-suggester, also rotated
    let srs = map (makeSuggestion hs') suggestions
    let kb' = execState (learnFromSuggestions srs) kb
    ppKB kb'
    print "all done!"