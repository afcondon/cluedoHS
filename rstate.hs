import Control.Monad.State

foo :: [Int] -> State Int [Int] 
foo [] = return []
foo (x:xs) = do
    i <- get
    put $ if x==5 then (i+1) else i
    r <- foo xs
    return $ (x*2):r

main = do
     let (lst,count) = runState (foo [1,2,5,6,5,5]) 0 in
         putStr $ show count