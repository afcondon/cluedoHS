# deductions
Player must have one card of any suggestion they matched
    ergo forall suggestions if two of the cards in a match are definitely not held third one is held

if you learn that a player doesn't have a card then you check their matches to see if there's information in that
if you learn that a player does have a card

-- would it be better to produce card facts here? probably many ways to skin the cat
deduceCard :: Suggestion -> CardSet -> CardSet -> Maybe Card
deduceCard (s,w,p) pc pc' 
    | (sb || wb || pb) = Nothing
    | (wb' && pb') = Just s
    | (sb' && pb') = Just w
    | (sb' && wb') = Just p
    | otherwise     = Nothing
    where
        sb = Set.member s pc
        wb = Set.member w pc
        pb = Set.member p pc
        sb' = Set.member s pc'
        wb' = Set.member w pc'
        pb' = Set.member p pc'

deduceCardS :: PlayerID -> Suggestion -> State KB ()
deduceCardS pid (s,w,p) = do
  let foo = pc.ix pid
  let foof = foo 
    | (sb || wb || pb) = Nothing
    | (wb' && pb') = Just s
    | (sb' && pb') = Just w
    | (sb' && wb') = Just p
    where
        sb = Set.member s pc
        wb = Set.member w pc
        pb = Set.member p pc
        sb' = Set.member s pc'
        wb' = Set.member w pc'
        pb' = Set.member p pc'
