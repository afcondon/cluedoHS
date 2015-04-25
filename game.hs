{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Text.Show.Pretty as Pr

data Game = Game
    { _score :: Int
    , _units :: [Unit]
    , _boss  :: Unit
    , _um :: Map.Map Int Char
    } deriving (Show)

data Unit = Unit
    { _health   :: Int
    , _position :: Point
    } deriving (Show)

data Point = Point
    { _x :: Double
    , _y :: Double
    } deriving (Show)

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point

initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    , _um = Map.fromList [(1,'a'), (2,'b')]
    }
ppGame :: Game -> IO ()
ppGame game = putStrLn $ Pr.ppShow game 

{-
-- this doesn't compile altho the original tutorial mentions problem
--  with filtered don't see why the signature would be wrong
around :: Point -> Double -> Traversal' Unit Unit
around center radius = filtered (\unit ->
    (unit^.position.x - center^.x)^2
  + (unit^.position.y - center^.y)^2
  < radius^2 )
-}

bossHP :: Lens' Game Int
bossHP = boss.health

strike                :: StateT           Game IO ()          {-
execStateT :: Monad m => StateT s m a  -> s            ->   m s
execStateT               strike        :: Game         ->   IO Game
execStateT               strike           initialState ::   IO game                    
newState :: game
  newState <- execStateT strike initialState                  -}
strike = do
    lift $ putStrLn "*shink*"
    bossHP -= 10
{-
mapmod :: StateT Game IO ()
mapmod =  do 
    lift $ putStrLn "mapmod"
    um & at 4 ?~ 'g'     -- foo^.um & at 4 ?~ 'v' within execState
-}

partyHP :: Traversal' Game Int
partyHP = units.traversed.health

fireBreath :: StateT Game IO ()
fireBreath = do
    lift $ putStrLn "*rawr*"
    units.traversed.health -= 3
{-
fireBreath :: Point -> StateT Game IO ()
fireBreath target = do
    lift $ putStrLn "*rawr*"
    units.traversed.(around target 1.0).health -= 3
-}

partyLoc :: Traversal' Game Point
partyLoc = units.traversed.position

retreat :: StateT Game IO ()
retreat = do
    lift $ putStrLn "Retreat!"
    zoom partyLoc $ do
        x += 10
        y += 10

battle :: StateT Game IO ()
battle = do
    -- Charge!
    forM_ ["Take that!", "and that!", "and that!"] $ \taunt -> do
        lift $ putStrLn taunt
        strike

    -- The dragon awakes!
    -- fireBreath (Point 0.5 1.5)
    fireBreath
    
    replicateM_ 3 $ do
        -- The better part of valor
        retreat

        -- Boss chases them
        zoom (boss.position) $ do
            x += 10
            y += 10

foo = initialState

main :: IO ()
main = print "game begins"
