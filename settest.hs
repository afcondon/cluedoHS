import Data.Ord
import Data.List
import Data.List.Split
import Data.Function (on)
import qualified Data.Set as S
import System.Random

data Card = MS | CM | MW | RG | MP | PP | Ck | Dg | Lp | Rp | Sp | Kn | Bl | Cn | Dr | Br | Lb | Lg | Hl | Sy deriving (Eq, Enum, Show, Ord)

-- | define the cards in the deck
suspectCards = [MS .. PP]
weaponCards = [Ck .. Sp]
placeCards = [Kn .. Sy]
-- | full deck
allCards = [MS .. Sy]
suggestions = [(s,w,p) | s <- suspectCards, w <- weaponCards, p <- placeCards]

type Hand = [Card]
type Deck = [Card]
type Shuffled = [Card]
type IntCardTuple = (Int, Card)
type Suggestion = (Card, Card, Card)
type PlayerID = Int
type SuggestionBy = (PlayerID, Suggestion)      -- actual suggestions are made by players
data Ownership = Unknown | PlayerID Int | Noone

type Fact = (Card, Ownership)

{-
    The idea is to make this a State Monad 

    Knowledge:
        Map from Card to Ownership
        Map from PID -> Set of Suggestions Matched
        Map from PID -> Set of Suggestions Passed
-}

-- | generic functions

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

initKB :: [Card] -> [Fact]
initKB [] = []
initKB (c:cs) = (c,Unknown) : initKB(cs)

-- | Globals 
d = allCards
s = shuffleDeck d

{- 
    Choose # of players
    parameterize random generator -- deferred for now
    Shuffle cards
    Take solution out of pack
    Deal hands out from remaining pack

    repeatedly test suggestions until solution found ()
    
        try each suggestion against each player until you get true or end of list 
        for each suggestion -> false add sugg' to player in question
            for each card in suggestion player does not have it - add this to KB
        for suggestion -> true (if any) add sugg to player
            IFF player.cards' contains two of the cards in the suggestion then player has remaining card - add fact to KB

        deduction:
            KB[card] = (card, value, )
                where
                    | 

matched :: Suggestion -> [(Bool, PlayerID)] -> [Fact]
matched [] = []
matched list
    | matchExists = [] -- we have learned that this player matched this suggestion
    | otherwise = [] -- possible murder card
    where
        last = tail list
        matchExists = fst last

learnFromPasses :: Suggestion -> [(Bool, PlayerID)] -> [Fact]
learnFromPasses s (hd:[]) = [] -- no information!
learnFromPasses s (hd:tl) = [] -- these players passed on this suggestion

learnFromMatch :: Suggestion -> [(Bool, PlayerID)] -> [Fact]
learnFromMatch s [] = _ -- none of the other players have any of these three cards (suggester could have any or all)
learnFromMatch s ((True, pid):_) = _  -- this player has one of these cards

-}

interpret :: [Bool] -> [PlayerID] -> IO ()
interpret results ps = do
    let info = zip results ps
    let passes = takeWhile (not . fst) info
    let match = dropWhile (not . fst) info -- head, if present, is match
    print info
    print passes
    print match
    

main = do
    putStrLn "How many players?"
    n <- readAInt -- number of players
    let ps = [1..n] :: [PlayerID]
    let mc = murderCards s
    let remainingCards = removeMurderCards mc s
    let hs = deal remainingCards n
    let results = makeSuggestion (head suggestions) hs  -- not hs but hs-without-suggester
    interpret results ps 
    print "all done!"