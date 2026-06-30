module Main exposing (main)

import Basics
import Browser
import Browser.Events as Events
import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..) 
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Wordlist exposing (..)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view        
        }
    
 -- MODEL
    
type alias Model =
    { answer : String
    , guesses : List EvaluatedGuess
    , currentGuess : String
    , gameState : GameState
    , shake : Bool
    , message : Maybe String
    }
    
    
init : () -> ( Model, Cmd Msg) 
init _ =
    ( { answer = defaultWord
     , guesses = []
     , currentGuess = ""
     , gameState = Playing
     , shake = False
     , message = Nothing 
     }
     , pickRandomWord
    )
    
pickRandomWord : Cmd Msg
pickRandomWord =
    Random.generate GotWord (Random.uniform defaultWord words)
    
 -- UPDATE 
    
type Msg
    = GotWord String
    | KeyPressed String
    | PressedEnter
    | PressedBackspace
    | PressedLetter Char
    | NewGame
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWord word ->
            ( { model | answer = String.toUpper word }, Cmd.none)

        KeyPressed key ->
            case key of
                "Enter" ->
                    update PressedEnter model
                
                "Backspace" ->
                    update PressedBackspace model
                    
                _ ->
                    case String.uncons key of
                        Just ( char, "") ->
                            if Char.isAlpha char then
                                update (PressedLetter char) model
                            else
                                ( model, Cmd.none)

                        _ -> ( model, Cmd.none)
                            
        PressedEnter -> (model, Cmd.none)
            

        PressedBackspace ->
            if model.gameState /= Playing then
                ( model, Cmd.none )
            else
                ( { model
                     | currentGuess = String.dropRight 1 model.currentGuess
                     , message = Nothing
                    }
                 , Cmd.none
                )
            
        PressedLetter char ->
            if model.gameState /= Playing then
                ( model, Cmd.none )
            else if String.length model.currentGuess < wordLength then
                ( { model
                    | currentGuess = model.currentGuess ++ String.fromChar (Char.toUpper char)
                    , message = Nothing
                    }
                , Cmd.none
                )
            else
                ( model, Cmd.none )
            
        NewGame ->
            let
                ( freshModel, _ ) =
                    init ()
            in
            ( freshModel, pickRandomWord )
            

 -- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onKeyDown keyDecoder
    
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map KeyPressed (Decode.field "key" Decode.string)


 -- VIEW

view : Model -> Html Msg
view model =
    div [ class "app"]
        [ viewHeader
        , viewMessage model.message
        , viewGrid model
        , viewKeyboard model
        , viewNewGame model
        ]


viewHeader : Html msg
viewHeader =
    header [ class "header" ]
        [ h1 [] [ text "WORDLE"] ]

viewMessage : Maybe String -> Html Msg
viewMessage maybeMessage =
    case maybeMessage of
       Nothing ->
           div [class "message-area"] []
       
       Just msg ->
           div [class "message-area"]
               [ div [ class "message"] [ text msg ] ]
           
            
viewCompletedRow : EvaluatedGuess -> Html Msg
viewCompletedRow guess =
    div [ class "row" ]
        (List.map viewEvaluatedCell guess)
        
viewGrid : Model -> Html Msg
viewGrid model =
    let
        completedRows =
            List.map viewCompletedRow model.guesses
        
        currentRow =
            if model.gameState == Playing then
                [ viewCurrentRow model.currentGuess model.shake ]
            else
                []
                
        emptyRowCount =
            maxGuesses - List.length model.guesses - (if model.gameState == Playing then 1 else 0)
         
        emptyRows =
             List.repeat (Basics.max 0 emptyRowCount) viewEmptyRow
             
    in
    div [ class "grid" ] (completedRows ++ currentRow ++ emptyRows)
    
viewCurrentRow : String -> Bool -> Html Msg
viewCurrentRow current shake =
    let
        chars =
            String.toList current
        
        filledCells =
           List.map
                (\c -> div [ class "cell active" ] [ text (String.fromChar c)])
                    chars
                    
        emptyCells =
            List.repeat (wordLength - List.length chars) (div [ class "cell" ] [])
    in
    div [ class ("row" ++ if shake then " shake" else "") ]
        (filledCells ++ emptyCells)
 
viewEmptyRow : Html Msg
viewEmptyRow =
    div [ class "row" ] 
        (List.repeat wordLength (div [ class "cell" ] []))
        
viewEvaluatedCell : ( Char, LetterState ) -> Html msg
viewEvaluatedCell ( char, state ) =
    div [ class ("cell " ++ letterStateClass state) ]
        [ text (String.fromChar char)]

letterStateClass : LetterState -> String
letterStateClass state =
    case state of
        Correct ->
            "correct"
        
        Present ->
            "present"

        Absent ->
            "absent"     

        Empty ->
            ""
                
 -- KEYBOARD VIEW
 
type alias KeyboardState =
    List ( Char, LetterState )
    
buildKeyboardState : List EvaluatedGuess -> KeyboardState
buildKeyboardState guesses =
    let
        priority s =
            case s of
                Correct -> 3
                Present -> 2
                Absent -> 1
                Empty -> 0
        
        updatedLetter ( char, newState ) kb =
            case List.filter (\(c, _) -> c == char) kb of
                [] ->
                    kb ++ [ ( char, newState ) ]
                (_, existingState ) :: _ ->
                    if priority newState > priority existingState then
                        List.map
                            (\( c, s) ->
                                if c == char then ( c, newState ) else ( c, s )
                            )
                            kb
                    else
                        kb
    in
    List.foldl
        (\guess kb -> List.foldl updatedLetter kb guess)
        []
        guesses
    
viewKeyboard : Model -> Html Msg
viewKeyboard model =
    let 
        kbState =
            buildKeyboardState model.guesses
            
        stateFor c =
            kbState
                |> List.filter (\( kc, _ ) -> kc == c)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Empty
                
        rows =
            [ String.toList "QWERTYUIOP"
            , String.toList "ASDFGHJKL"
            , String.toList "ZXCVBNM"
            ]
    in
    div [ class "keyboard" ]
        (List.indexedMap
            (\i row ->
                div [ class "keyboard-row" ]
                    ((if i == 2 then [ viewSpecialKey "ENTER" PressedEnter ] else [])
                    ++ List.map (\c -> viewKey c (stateFor c)) row
                    ++ (if i == 2 then [ viewSpecialKey "⌫" PressedBackspace ] else [] )
                    )
            )
            rows
        )

viewKey : Char -> LetterState -> Html Msg
viewKey char state =
    button [class ("key " ++ letterStateClass state)
    , onClick (PressedLetter char)
    ]
    [ text (String.fromChar char) ]
    
viewSpecialKey : String -> Msg -> Html Msg
viewSpecialKey label msg =
    button
        [ class "key key-wide", onClick msg ] [text label]

viewNewGame : Model -> Html Msg
viewNewGame model =
    if model.gameState == Playing then
        text ""
    else
        div [ class "new-game" ]
            [ button [ class "btn-new-game", onClick NewGame ] [ text "New Game" ] ]
    
    