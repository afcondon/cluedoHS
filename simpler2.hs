{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}

import Control.Lens
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type CharSet = Set.Set Char
type MapOfSets = Map.Map Int CharSet

data DB = DB { _myMap  :: MapOfSets} deriving (Show, Eq)

makeLenses ''DB

initDB :: DB
initDB =  DB { _myMap  = Map.fromList (zip [1..5] (repeat Set.empty)) }

add2Map :: Int -> CharSet -> State DB ()
add2Map i cs = myMap.ix i %= (Set.union cs)

traverseAll :: Traversal' DB CharSet
traverseAll = myMap.traversed

add2MapsAll :: CharSet -> State DB ()
add2MapsAll cs = traverseAll %= (Set.union cs)

traverseSome :: [Int] -> Int -> Traversal' DB MapOfSets
traverseSome i is = _

add2MapsSome :: [Int] -> CharSet -> State DB ()
add2MapsSome is cs = traverseAll %= (Set.union cs)

{-
around :: Point -> Double -> Traversal' Unit Unit
around center radius = 
  filtered (\unit -> (unit^.position.x - center^.x)^2 + (unit^.position.y - center^.y)^2 < radius^2 )

fireBreath :: Point -> StateT Game IO ()
fireBreath target = do
    lift $ putStrLn "*rawr*"
    units.traversed.(around target 1.0).health -= 3
-}

bar = Set.fromList ['a'..'g'] :: CharSet
baz = Set.fromList ['f'..'m'] :: CharSet
quux = Set.fromList ['n'..'z'] :: CharSet

main :: IO ()
main = do
  let db = initDB
  let db2 = execState (add2Map 1 bar) db
  let db3 = execState (add2MapsAll baz) db
  -- let db4 = execState (add2MapsSome [1,3] quux) db
  print db2
  print db3
  -- print db4
