import qualified Data.Set as S
import Control.Monad.State

inc :: State Int Int
inc = do
    n <- get
    put (n + 1)
    return n

incBy :: Int -> State Int Int
incBy x = do
    n <- get
    modify (+x)
    return n

recurringIncBy :: Int -> State Int Int
recurringIncBy x = do
    n <- get
    modify (+x)
    modify (+x)
    return n

recursingInc :: State Int Int
recursingInc = do
    n <- get
    if (n == 0) then return 1 else return $ (+ 2) $ execState recursingInc (n - 1) 

type StateSet = S.Set Int

addToSet :: Int -> State StateSet StateSet
addToSet n = do
    s <- get
    put (S.insert n s)
    return s

foo :: State [Int] StateSet
foo = do
    is <- get
    modify (take 2)
    return $ S.fromList is

main = do
    print $ evalState inc 1
    print $ execState inc 1
    print $ runState inc 1
    print $ runState (withState (+3) inc) 1
    print $ runState (mapState (\(a, s) -> (a + 3, s + 4)) inc) 1

    print $ (flip runState) 0  $ do
        incBy 1
        incBy 2
        incBy 3

