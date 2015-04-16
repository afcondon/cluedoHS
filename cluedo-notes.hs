
{- 
    Choose # of players
    parameterize random generator -- deferred for now
    Shuffle cards
    Take solution out of pack
    Deal hands out from remaining pack

    repeatedly test suggestions until solution found ()
    
        try each suggestion against each player until you get true or end of list 
        for each suggestion -> false add sugg' to player in question
            for each card in suggestion player does not have it - add this to KB
        for suggestion -> true (if any) add sugg to player
            IFF player.cards' contains two of the cards in the suggestion then player has remaining card - add fact to KB

        deduction:
            KB[card] = (card, value, )
                where
                    | 
-}

matched :: Suggestion -> [(Bool, PlayerID)] -> [Fact]
matched [] = []
matched list
    | matchExists = [] -- we have learned that this player matched this suggestion
    | otherwise = [] -- possible murder card
    where
        last = tail list
        matchExists = fst last

learnFromMatch :: Suggestion -> [(Bool, PlayerID)] -> [Fact]
learnFromMatch s [] = _ -- none of the other players have any of these three cards (suggester could have any or all)
learnFromMatch s ((True, pid):_) = _  -- this player has one of these cards

{-
    The idea is to make this a State Monad 

    Knowledge:
        Map from Card to Ownership
        list of Map from PID -> Set of Suggestions Matched
        List of Map from PID -> Set of Suggestions Passed
        List of map from PID -> Set of Cards Held
        List of map from PID -> Set of Cards NotHeld
-}
