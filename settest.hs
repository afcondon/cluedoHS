{-# LANGUAGE DeriveGeneric #-}

import Data.Ord
import Data.List
import Data.List.Split
import Data.Function (on)
import System.Random
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.Show.Pretty as Pr

-- | define the cards in the deck
-------------------------------------------------------------------------------
data Card = MS | CM | MW | RG | MP | PP | Ck | Dg | Lp | Rp | Sp | Kn | Bl | Cn | Dr | Br | Lb | Lg | Hl | Sy deriving (Eq, Enum, Show, Ord)
type Suggestion = (Card, Card, Card) -- constrained to be (s,w,p)
type Hand = [Card]
type Deck = [Card]
type Shuffled = [Card]
type IntCardTuple = (Int, Card)
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

makeSuggestion :: Suggestion -> [Hand] -> [Bool]
makeSuggestion _ [] = []
makeSuggestion s (h:hs) = testSuggestion s h : makeSuggestion s hs

-- core information used for deduction and tracking
-------------------------------------------------------------------------------
type CardSet = S.Set Card
type SuggestionSet = S.Set Suggestion
data Ownership = Unknown | Noone | PlayerID deriving (Show, Eq)

type CardFacts = M.Map Card Ownership
type PlayerCards = M.Map PlayerID CardSet
type PlayerCards' = M.Map PlayerID CardSet
type PlayerPassed = M.Map PlayerID SuggestionSet
type PlayerMatched = M.Map PlayerID SuggestionSet

data KB = KB { cf :: CardFacts
             , pc :: PlayerCards
             , pc' :: PlayerCards'
             , pm :: PlayerMatched
             , pm' :: PlayerPassed } deriving (Show, Eq)

--instance Out KB

initCardFacts :: CardFacts
initCardFacts = M.fromList (zip allCards (repeat Unknown))

initCardSet :: CardSet
initCardSet = S.empty

initSuggestionSet :: SuggestionSet
initSuggestionSet = S.empty

initKB :: Int -> KB
initKB n = KB { cf  = initCardFacts
              , pc  = M.fromList $ zip [1..n] $ repeat initCardSet
              , pc' = M.fromList $ zip [1..n] $ repeat initCardSet
              , pm  = M.fromList $ zip [1..n] $ repeat initSuggestionSet
              , pm' = M.fromList $ zip [1..n] $ repeat initSuggestionSet }



getPasses :: [(Bool, PlayerID)] -> [PlayerID]
getPasses info = map snd $ takeWhile (not . fst) info

getMatch :: [(Bool, PlayerID)] -> Maybe PlayerID
getMatch [] = Nothing
getMatch ((True, pid):_) = Just pid
getMatch ((False, _):ps) = getMatch ps

learnFromSuggestion :: [PlayerID] -> Maybe PlayerID -> IO ()
learnFromSuggestion passes match = do
    print passes
    print match  

-- | Globals 
-------------------------------------------------------------------------------
d = allCards
s = shuffleDeck d

main = do
    putStrLn "How many players?"
    n <- readAInt -- number of players
    let ps = [1..n] :: [PlayerID]
    let kb = initKB n
    let mc = murderCards s
    let remainingCards = removeMurderCards mc s
    let hs = deal remainingCards n
    let firstS = head suggestions
    let results = makeSuggestion firstS hs  -- not hs but hs-without-suggester
    let info = zip results ps
    let passes = getPasses info
    let match = getMatch info 
    print info
    putStrLn $ Pr.ppShow kb 
    --print kb
    learnFromSuggestion passes match
    print "all done!"