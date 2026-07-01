module Game exposing 
    ( GameState(..)
    , LetterState(..) 
    , EvaluatedGuess
    , evaluateGuess
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
    
evaluateGuess : String -> String -> EvaluatedGuess
evaluateGuess answer guess =
    let
        answerChars = String.toList (String.toUpper answer)
        
        guessChars = String.toList (String.toUpper guess)
        
        correctPass = 
            List.map2 (
                \a g ->
                    if a == g then ( g, Just Correct )
                    else ( g, Nothing )
            ) answerChars guessChars
            
        correctChars =
            List.map2 (\a g -> if a == g then Just a else Nothing)
                answerChars
                guessChars
                |> List.filterMap identity
       
        remainingBudget =
            List.foldl (
                \c budget ->
                    removeFirst c budget
            ) answerChars correctChars
            
        (_, finalStates ) =
            List.foldl (\( g, maybeState ) (budget, acc) ->
                case maybeState of
                    Just state ->
                        ( budget, acc ++ [ ( g, state) ] )
                    
                    Nothing ->
                        if List.member g budget then
                            ( removeFirst g budget, acc ++ [ (g, Present ) ] )
                        else
                            ( budget, acc ++ [ ( g, Absent ) ] )
                )
                ( remainingBudget, [] )
                correctPass
    in
    finalStates
        

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
