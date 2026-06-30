module Game exposing 
    ( GameState(..)
    , LetterState(..) 
    , EvaluatedGuess
    , hasWon
    , hasLost
    , maxGuesses
    , wordLength
    )

maxGuesses : Int
maxGuesses =
    6
    
wordLength : Int
wordLength =
    5
    
type LetterState
    = Correct
    | Present
    | Absent
    | Empty    

type GameState
    = Playing
    | Won
    | Lost 
    
type alias EvaluatedGuess =
    List (Char, LetterState)
    

removeFirst : a -> List a -> List a
removeFirst target list =
    case list of
        [] ->
            []
        x :: xs ->
            if x == target then
                xs
            else
                x :: removeFirst target xs
    
hasWon : List EvaluatedGuess -> Bool
hasWon guesses =
    case List.reverse guesses of
        [] ->
            False
        last :: _ ->
            List.all (\(_, state )-> state == Correct) last

hasLost : List EvaluatedGuess -> Bool
hasLost guesses =
    List.length guesses >= maxGuesses && not (hasWon guesses) 
