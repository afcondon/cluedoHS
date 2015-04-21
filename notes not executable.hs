-- *Main> :t cf
cf :: Functor f => (CardFacts -> f CardFacts) -> KB -> f KB

-- *Main> :t (^.)
(^.) :: s -> Getting a s a -> a

-- *Main> :t foo
foo :: KB

-- *Main> :t foo^.cf
foo^.cf :: CardFacts


-- view cf foo 			is exactly equal to 		foo^.cf

-- *Main> :t view
MonadReader s m => Getting a s a -> m a

-- this is legal
foo & cf .~ initCardFacts

-- but this isn't 
foo^.cf .~ initCardFacts 

Map.fromList [("hello",12)] ^.at "hello"
Just 12

at 10 .~ Just "hello" $ Map.empty
fromList [(10,"hello")]


-- *Main> let bar = foo^.pc
-- *Main> at 1 .~ Just initCardSet $ bar
fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])]

-- so we might try something like this:
foo^.pc^.at 1 .~ Just initCardSet 
-- but that doesn't work

-- this however does:
-- *Main> foo^.pc^.at 1
Just (fromList [])
-- *Main> foo^.pc^.at 8
Nothing