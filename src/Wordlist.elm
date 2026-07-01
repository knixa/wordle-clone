module Wordlist exposing (words, defaultWord, isValidWord)

defaultWord : String
defaultWord = "CRANE"
words : List String
words =
    [ "SLATE", "AUDIO", "RAISE", "STARE"
    , "TRACE", "CRATE", "STORE", "SNARE", "SHARE"
    , "SHORE", "SCORE", "SWORE", "SPARE", "SPORE"
    , "BLAZE", "CLAMP", "DRAFT", "FLOOD", "GLINT"
    , "HAPPY", "JUMPY", "KNACK", "LYMPH", "MIXED"
    , "NYMPH", "OXIDE", "PERKY", "QUACK", "ROVER"
    , "SKIMP", "TROVE", "ULCER", "VIXEN", "WALTZ"
    , "PROXY", "YACHT", "ZEBRA", "ABBEY", "SPEED"
    ]
   
  
isValidWord : String -> Bool
isValidWord word =
    List.member (String.toUpper word) words
