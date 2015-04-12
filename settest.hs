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

data CardOwner = Noone | PlayerID Int | Unknown
data CardInfo = CardInfo { c :: Card, v :: CardOwner, eliminated :: [PlayerID]}
type CardKB = [CardInfo] -- really this should be a Map from Card to CardInfo?

data PlayerInfo = PlayerKB { ss :: [Suggestion], ss' :: [Suggestion] } deriving (Show)
type PlayerKB = [PlayerInfo] -- should be a Map from PlayerID to PlayerInfo

initCardKB :: Shuffled -> Int -> CardKB
initCardKB s n = [CardInfo {c = card, v = Unknown, eliminated = [] }  | card <- s]

emptyPlayer = PlayerInfo { ss = [], ss' = [] }

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

pruneSuggestionFromShuffled :: Suggestion -> Shuffled -> Shuffled
pruneSuggestionFromShuffled (sc,wc,pc) s = filter (/= sc) $ filter (/= wc) $ filter (/= pc) s

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

getPlayers :: Int -> [PlayerModel]
getPlayers n = take n $ repeat emptyPlayer

rounds :: [Hand] -> [Hand]
rounds hs = cycle hs

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
-}



main = do
    putStrLn "How many players?"
    n <- readAInt -- number of players
    let ps = getPlayers n
    let mc = murderCards s
    let remainingCards = pruneSuggestionFromShuffled mc s
    let hs = deal remainingCards n
    let results = makeSuggestion (head suggestions) hs
    print results

