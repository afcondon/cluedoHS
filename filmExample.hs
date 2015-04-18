{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens

type Title = String
type Actor = String
type Cast = [Actor]
type Year = Int
type Fan = String
type Fans = [Fan]
type Period = (Year, Year)

type Name = String
data Film = Film { _title :: Title, _cast :: Set.Set Name, _year :: Year, _fans :: Set.Set Name } deriving (Show)

makeLenses ''Film

type Database = Map.Map Title Film


testDatabase :: Database
testDatabase = Map.fromList [ ("Casino Royale", Film { _title = "Casino Royale", _cast = Set.fromList ["Daniel Craig", "Eva Green", "Judi Dench"], _year = 2006, _fans = Set.fromList ["Garry", "Dave", "Zoe", "Kevin", "Emma"]}),
                              ("Cowboys & Aliens", Film { _title = "Cowboys & Aliens", _cast = Set.fromList ["Harrison Ford", "Daniel Craig", "Olivia Wilde"], _year = 2011, _fans = Set.fromList ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"]}),     
                              ("Catch Me If You Can", Film { _title = "Catch Me If You Can", _cast = Set.fromList ["Leonardo DiCaprio", "Tom Hanks"], _year = 2002, _fans = Set.fromList ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"]}),
                              ("Mamma Mia!", Film { _title = "Mamma Mia!", _cast = Set.fromList ["Meryl Streep", "Pierce Brosnan"], _year = 2008, _fans = Set.fromList ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"]})]
{-
becomeFan1 name title = 
  Map.alter update title where
    update Nothing  = Nothing -- that title was not in our database
    update (Just f) = Just (f { fans = Set.insert name (fans f) })

newDB1 = becomeFan1 "Andrew" "Catch Me If You Can" testDatabase

-}

becomeFan2 :: Fan -> Title -> (Database -> Database)
becomeFan2 name title = over (at title) (fmap . over fans . Set.insert $ name)




catchMe = Map.lookup "Catch Me If You Can"


