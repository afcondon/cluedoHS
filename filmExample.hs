import qualified Data.Set as Set
import qualified Data.Map as Map

type Title = String
type Actor = String
type Cast = [Actor]
type Year = Int
type Fan = String
type Fans = [Fan]
type Period = (Year, Year)

type Name = String
data Film = Film { title :: Title, cast :: Set.Set Name, year :: Year, fans :: Set.Set Name } deriving (Show)
type Database = Map.Map Title Film


testDatabase :: Database
testDatabase = Map.fromList [ ("Casino Royale", Film { title = "Casino Royale", cast = Set.fromList ["Daniel Craig", "Eva Green", "Judi Dench"], year = 2006, fans = Set.fromList ["Garry", "Dave", "Zoe", "Kevin", "Emma"]}),
                              ("Cowboys & Aliens", Film { title = "Cowboys & Aliens", cast = Set.fromList ["Harrison Ford", "Daniel Craig", "Olivia Wilde"], year = 2011, fans = Set.fromList ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"]}),     
                              ("Catch Me If You Can", Film { title = "Catch Me If You Can", cast = Set.fromList ["Leonardo DiCaprio", "Tom Hanks"], year = 2002, fans = Set.fromList ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"]}),
                              ("Mamma Mia!", Film { title = "Mamma Mia!", cast = Set.fromList ["Meryl Streep", "Pierce Brosnan"], year = 2008, fans = Set.fromList ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"]})]

becomeFan name title = 
  Map.alter update title where
    update Nothing  = Nothing -- that title was not in our database
    update (Just f) = Just (f { fans = Set.insert name (fans f) })

newDB = becomeFan "Andrew" "Catch Me If You Can" testDatabase
catchMe = Map.lookup "Catch Me If You Can"


