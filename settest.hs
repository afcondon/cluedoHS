import Data.Ord
import Data.List
import Data.List.Split
import Data.Function (on)
import Data.Set (Set)
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

-- data PlayerModel = { Cards, Cards', SuggestionsSatisfied, SuggestionsPassed }
-- emptyPlayer = PlayerModel { [], [], [], [] }

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

makeSuggestion :: Suggestion -> Hand -> Bool
makeSuggestion (s,w,p) h = (s `elem` h) || (w `elem` h) || (p `elem` h)

-- getPlayers :: Int -> [PlayerModel]
-- getPlayers n = repeat n emptyPlayer

-- | Globals 
n = 4 -- number of players
d = allCards
s = shuffleDeck d
hs = deal s n

rounds = cycle hs -- | infinite list 

main = do
	print hs
	print "hello world of sets"