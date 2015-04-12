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

data PlayerModel = PlayerModel {  cs :: [Card]
								, cs' :: [Card]
								, ss :: [Suggestion]
								, ss' :: [Suggestion] } deriving (Show)

emptyPlayer = PlayerModel { cs = [], cs' = [], ss = [], ss' = [] }

shuffle :: [a] -> [a]
shuffle as = snd $ unzip slist
	where
		rlist = randomRs (0, length as) (mkStdGen 42)
		slist = sortBy (comparing fst) $ zip rlist as

-- | equations
shuffleDeck :: Deck -> Shuffled
shuffleDeck d = shuffle d

deal :: Shuffled -> Int -> [Hand]
deal deck n = map (map snd) $ groupBy ((==) `on` fst) $ sortBy (comparing fst) ts
	where
		ts = zip (cycle [1..n]) deck

testSuggestion :: Suggestion -> Hand -> Bool
testSuggestion (s,w,p) h = or bools
	where
		asList = [s,w,p]
		bools = map (flip elem asList) h

getPlayers :: Int -> [PlayerModel]
getPlayers n = take n $ repeat emptyPlayer

rounds :: [Hand] -> [Hand]
rounds hs = cycle hs

-- | Globals 
d = allCards
s = shuffleDeck d

-- | useful for debugging
--foo = head suggestions
--bar = head hs
--hs = deal s n

readAInt :: IO Int
readAInt = readLn


main = do
	putStrLn "How many players?"
	n <- readAInt -- number of players
	print $ deal s n
