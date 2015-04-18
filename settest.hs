{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Ord
import Data.List
import Data.List.Split
import Data.Function (on)
import System.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.Show.Pretty as Pr


----------------------------------------
type Stack   = [Int]
type Output  = [Int]
type Program = [Instr]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a = Comp { unComp :: VM a }
  deriving (Functor, Applicative, Monad, MonadReader Program, MonadWriter Output, MonadState Stack)

data Instr = Push Int | Pop | Puts

evalInstr :: Instr -> Comp ()
evalInstr instr = case instr of
  Pop    -> modify tail
  Push n -> modify (n:)
  Puts   -> do
    tos <- gets head
    tell [tos]

eval :: Comp ()
eval = do
  instr <- ask
  case instr of
    []     -> return ()
    (i:is) -> evalInstr i >> local (const is) eval

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval)

program :: Program
program = [
     Push 42,
     Push 27,
     Puts,
     Pop,
     Puts,
     Pop
  ]
---------------------------------------------------------------------
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

type KBstate = State KB

--updateKB :: KBstate Card Ownership
--updateKB c o = do 
--    st <- get
--    let newCF = M.Map insert c o
--    put st { cf = newCF, pc = pc}
--    return l


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

-- getters and setters pending use of lenses or state transformers
addCardFact :: (Card, Ownership) -> KB -> KB
addCardFact (c, o) kb = KB { cf = cfN, pc = pcN, pc' = pc'N, pm = pmN, pm' = pm'N }
    where
        cfN = M.insert c o (cf kb)
        pcN = pc kb
        pc'N = pc' kb
        pmN = pm kb
        pm'N = pm' kb
{-
playerDoesNotHaveCard :: KB -> PlayerID -> Card -> KB
playerDoesNotHaveCard kb pid card = KB { cf = cfN, pc = pcN, pc' = pc'N, pm = pmN, pm' = pm'N }
    where
        cfN = cf kb
        csN = S.insert card (M.lookup pid (pc kb))
        pcN = M.insert pid csN (pc kb) :: PlayerCards
        pc'N = pc' kb
        pmN = pm kb
        pm'N = pm' kb
-}

getPasses :: [(Bool, PlayerID)] -> [PlayerID]
getPasses info = map snd $ takeWhile (not . fst) info

getMatch :: [(Bool, PlayerID)] -> Maybe PlayerID
getMatch [] = Nothing
getMatch ((True, pid):_) = Just pid
getMatch ((False, _):ps) = getMatch ps
{-
learnFromSuggestion :: KB -> Suggestion -> [PlayerID] -> Maybe PlayerID -> KB
learnFromSuggestion kb (s,w,p) passes match = newKB
    where
        newKB = playerDoesNotHaveCard s (head passes)
-}

-- | Globals 
-------------------------------------------------------------------------------
d = allCards
s = shuffleDeck d

main = do
    mapM_ print $ execVM program
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
    let cfN = cf kb
    let csn = M.lookup 1 (pc kb)
    print csn
    let foo = case csn of Nothing -> initCardSet
                          (Just c) -> S.insert MS c
    print foo
    --print kb
    -- learnFromSuggestion firstS passes match
    print "all done!"