{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Lens
-- import Data.Monoid
import Control.Monad.State
import qualified Data.Map.Strict as Map

data DB = DB { _myMap  :: Map.Map Int Char} deriving (Show, Eq)

makeLenses ''DB

initDB :: DB
initDB =  DB { _myMap  = foo }

add2Map :: Int -> Char -> State DB ()
add2Map i c = myMap.at i .= (Just c)
{-
add2MapMultiple :: Map.Map Int Char -> State DB ()
add2MapMultiple mmc = myMap <>= mmc
-}


add2MapMultiple2 :: Map.Map Int Char -> State DB ()
add2MapMultiple2 mmc = myMap %= (Map.union mmc)


db = initDB
foo = Map.fromList (zip [1..5] ['a'..'m']) :: Map.Map Int Char
bar = Map.fromList (zip [4..9] ['n'..'z'])  :: Map.Map Int Char

main :: IO ()
main = do
  let ndb = execState (add2Map 1 'z') db
  -- let ndb2 = execState (add2MapMultiple bar) ndb
  let ndb3 = execState (add2MapMultiple2 bar) ndb
  print ndb
  -- print ndb2
  print ndb3
