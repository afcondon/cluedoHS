*Main > :t cf
cf :: Functor f => (CardFacts -> f CardFacts) -> KB -> f KB

*Main > :t (^.)
(^.) :: s -> Getting a s a -> a

*Main > :t foo
foo :: KB

*Main > :t foo^.cf
foo^.cf :: CardFacts

-- view cf foo 			is exactly equal to 		foo^.cf

*Main > :t view
MonadReader s m => Getting a s a -> m a

-- this is legal
*Main > foo & cf .~ initCardFacts

-- but this isn't 
-- foo^.cf .~ initCardFacts 

*Main > Map.fromList [("hello",12)] ^.at "hello"
Just 12

*Main > at 10 .~ Just "hello" $ Map.empty
fromList [(10,"hello")]

------------------------------------------------------------------------------

*Main > let bar = foo^.pc
*Main > at 1 .~ Just initCardSet $ bar
fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])]

-- so we might try something like this:
foo^.pc^.at 1 .~ Just initCardSet 
-- but that doesn't work

-- this however does:
-- *Main> foo^.pc^.at 1
Just (fromList [])
-- *Main> foo^.pc^.at 8
Nothing

-- *Main> ix 3 %~ (map toUpper) $ Map.fromList [(2, "Earth"), (3, "Mars")]
fromList [(2,"Earth"),(3,"MARS")]
-- *Main> ix 3 .~ "foo"  $ Map.fromList [(2, "Earth"), (3, "Mars")]
fromList [(2,"Earth"),(3,"foo")]

------------------------------------------------------------------------------

-- working from the simple example this is legit
*Main> db^.fs & at 3 ?~ 'x'
fromList [(1,'a'),(2,'b'),(3,'x'),(4,'d'),(5,'e')]

-- and so this works
*Main> db^.cf & at 'a' ?~ Noone
fromList [('a',Noone),('b',Unknown),('c',Unknown),('d',Unknown)]

*Main > db^.cf & at 'a' ?~ ID 5
fromList [('a',ID 5),('b',Unknown),('c',Unknown),('d',Unknown)]

-- here's an example of a nested map insertion
*Main > Map.empty & at "hello" . non Map.empty . at "world" ?~ "!!!"
fromList [("hello",fromList [("world","!!!")])]

------------------------------------------------------------------------------

*Main > foo^.um.at 1
Nothing

*Main > foo^.um & at 1 ?~ 's'
fromList [(1,'s')]
-- but
*Main > foo^.um.at 1 ?~ 's'
-- <bombs>

*Main > at 1 ?~ 'j' $ foo^.um
*Main > foo^.myMap & at 1 ?~ 'j'
fromList [(1,'j'),(2,'b')]

*Main > foo^.myList & ix 1 .~ 5
[1,5,3,4,5,6,7,8,9,10]

------------------------------------------------------------------------------

>>> let m = M.fromList [('a',1), ('b',2), ('c',3)]
>>> let k = S.fromList ['b','c','e']
>>> m ^.. foldMap at k
[Just 2,Just 3,Nothing]
>>> m ^.. foldMap ix k
[2,3]

------------------------------------------------------------------------------

*Main > fes foo (cf.at MS .= (Just Noone))
KB {_cf = fromList [(MS,Noone),(CM,Unknown),(MW,Unknown),(RG,Unknown),(MP,Unknown),(PP,Unknown),(Ck,Unknown),(Dg,Unknown),(Lp,Unknown),(Rp,Unknown),
(Sp,Unknown),(Kn,Unknown),(Bl,Unknown),(Cn,Unknown),(Dr,Unknown),(Br,Unknown),(Lb,Unknown),(Lg,Unknown),(Hl,Unknown),(Sy,Unknown)],
 _pc = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])], 
 _pcl = fromList [(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[])], _pc' = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),
 (5,fromList []),(6,fromList [])], 
 _pm = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])], 
 _pm' = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])]}

*Main > fes foo (cf.at MS .= (Just (ID 1)))
KB {_cf = fromList [(MS,ID 1),(CM,Unknown),(MW,Unknown),(RG,Unknown),(MP,Unknown),(PP,Unknown),(Ck,Unknown),(Dg,Unknown),(Lp,Unknown),(Rp,Unknown),
(Sp,Unknown),(Kn,Unknown),(Bl,Unknown),(Cn,Unknown),(Dr,Unknown),(Br,Unknown),(Lb,Unknown),(Lg,Unknown),(Hl,Unknown),(Sy,Unknown)], 
_pc = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])], 
_pcl = fromList [(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[])], _pc' = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),
(5,fromList []),(6,fromList [])], 
_pm = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])], 
_pm' = fromList [(1,fromList []),(2,fromList []),(3,fromList []),(4,fromList []),(5,fromList []),(6,fromList [])]}

-- add2Map using state transformer
add2MapT :: Int -> Char -> StateT DB IO ()
add2MapT i c = myMap.at i .= (Just c)

-- i worked this out from the types, i did!
*Main> foo
fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
*Main> bar
fromList [(4,'n'),(5,'o'),(6,'p'),(7,'q'),(8,'r'),(9,'s')]
*Main> over myMap (Map.union bar) db
DB {_myMap = fromList [(1,'a'),(2,'b'),(3,'c'),(4,'n'),(5,'o'),(6,'p'),(7,'q'),(8,'r'),(9,'s')]}

