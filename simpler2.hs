{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}

import Control.Lens
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type CharSet = Set.Set Char
type MapOfSets = Map.Map Int CharSet

data DB = DB { _mos  :: MapOfSets } deriving (Show, Eq)

makeLenses ''DB

initDB :: DB
initDB =  DB { _mos  = Map.fromList (zip [1..5] (repeat Set.empty)) }

add2Map :: Int -> CharSet -> State DB ()
add2Map i cs = mos.ix i %= (Set.union cs)

traverseAll :: Traversal' DB CharSet
traverseAll = mos.traversed

add2MapsAll :: CharSet -> State DB ()
add2MapsAll cs = traverseAll %= (Set.union cs)

--        <problematic part>          
{-
traverseSome :: [Int] -> Int -> Traversal' DB MapOfSets
traverseSome i is = _

add2MapsSome :: [Int] -> CharSet -> State DB ()
add2MapsSome is cs = (traverseSome is _) %= (Set.union cs)
-}         
--        </problematic part>

main :: IO ()
main = do
  let db = initDB
  let bar = Set.fromList ['a'..'g'] :: CharSet
  let baz = Set.fromList ['f'..'m'] :: CharSet
  let quux = Set.fromList ['n'..'z'] :: CharSet

  let db2 = execState (add2Map 5 bar) db
  let db3 = execState (add2MapsAll baz) db
  -- let db4 = execState (add2MapsSome [1,3] quux) db

  print db2
  print db3
  -- pri nt db4
