{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Ord
import Data.List
import Data.List.Split
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

testSuggestion :: Suggestion -> Hand -> Bool
testSuggestion (s,w,p) h = or bools
    where
        asList = [s,w,p]
        bools = map (flip elem asList) h

getPasses :: [(Bool, PlayerID)] -> [PlayerID]
getPasses info = map snd $ takeWhile (not . fst) info

getMatch :: [(Bool, PlayerID)] -> Maybe PlayerID
getMatch [] = Nothing
getMatch ((True, pid):_) = Just pid
getMatch ((False, _):ps) = getMatch ps

evalSuggestion :: Suggestion -> [Hand] -> [Bool]
evalSuggestion _ [] = []
evalSuggestion s (h:hs) = testSuggestion s h : evalSuggestion s hs

makeSuggestion :: Suggestion -> [Hand] -> SuggestionResult
makeSuggestion s hs = (passes, match)
  where
    results = zip (evalSuggestion s hs) [1..(length hs)]
    passes = getPasses results
    match  = getMatch results


-- core information used for deduction and tracking
-------------------------------------------------------------------------------
type CardSet = Set.Set Card
type SuggestionSet = Set.Set Suggestion
data Ownership = Unknown | Noone | ID PlayerID deriving (Show, Eq)

type CardFacts = Map.Map Card Ownership
type PlayerCards = Map.Map PlayerID CardSet
type PlayerCards' = Map.Map PlayerID CardSet
type PlayerPassed = Map.Map PlayerID SuggestionSet
type PlayerMatched = Map.Map PlayerID SuggestionSet
type SuggestionResult = ([PlayerID], Maybe PlayerID)

initCardFacts :: CardFacts
initCardFacts = Map.fromList (zip allCards (repeat Unknown))

initCardSet :: CardSet
initCardSet = Set.empty

initSuggestionSet :: SuggestionSet
initSuggestionSet = Set.empty

data KB = KB { _cf  :: CardFacts
             , _pc  :: PlayerCards   -- Map Player -> Set of Cards held
             , _pc' :: PlayerCards' -- Map Player -> Set of Cards _not_ held
             , _pm  :: PlayerMatched -- Map Player -> Set of Suggs Matched
             , _pm' :: PlayerPassed -- Map Player -> Set of Suggs Passed
             } deriving (Show, Eq)

initKB :: Int -> KB
initKB n = KB { _cf  = initCardFacts
              , _pc  = Map.fromList $ zip [1..n] $ repeat initCardSet
              , _pc' = Map.fromList $ zip [1..n] $ repeat initCardSet
              , _pm  = Map.fromList $ zip [1..n] $ repeat initSuggestionSet
              , _pm' = Map.fromList $ zip [1..n] $ repeat initSuggestionSet }

makeLenses ''KB

ppKB :: KB -> IO ()
ppKB kb = putStrLn $ Pr.ppShow kb 


addCardFact :: Card -> Ownership -> State KB ()
addCardFact c o = cf.at c .= Just o

addCardFacts :: CardFacts -> State KB ()
addCardFacts cfs = cf %= (Map.union cfs)

{-
passers :: [PlayerID] -> Traversal' PlayerPassed PlayerPassed
passers ps = filtered (Map.member ps)

learnFromSuggestion :: Suggestion -> SuggestionResult -> State KB ()
learnFromSuggestion sugg (ps,m) = do
  pm'.traversed.(passers ps) %= (Set.union sugg)

reviewCardFacts :: State KB () 

reviewSuggestions :: State KB ()
-}

solved :: KB -> Maybe Suggestion       -- should be an Either since there is distinct error condition
solved kb =  case swp of
            []        -> Nothing       -- no solution is found
            (x:[])    -> Just x        -- single solution is found
            otherwise -> Nothing       -- multiple solutions found, algo is broken  
  where
    cfs = Map.toList $ kb^.cf
    swp = [ (s,w,p) | (s, Noone) <- cfs, (w, Noone) <- cfs, (p, Noone) <- cfs, elem s suspectCards, elem w weaponCards, elem p placeCards]

-- | Globals 
-------------------------------------------------------------------------------
d = allCards
s = shuffleDeck d
foo = initKB 6

main = do
    putStrLn "How many players?"
    n <- readAInt -- number of players
    let ps = [1..n] :: [PlayerID]
    let kb = initKB n
    let mc = murderCards s
    let remainingCards = removeMurderCards mc s
    let hs = deal remainingCards n
    let firstS = head suggestions
    let sResult = makeSuggestion firstS hs  -- not hs but hs-without-suggester
    -- let results = execState (learnFromSuggestion firstS sResult) kb
    -- ppKB results
    print "all done!"