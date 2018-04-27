module Scanner where

import qualified Data.List as L
import Utils

data TokenCoq
  = SimpleComment String
  | CoqDoc String
  | CodeChunk String
  deriving Show


{-
Structural tokens in V files
- Title
    * Basics: Functional Programming in Coq
    **** Exercise: 1 star (nandb)

    The first the main title, and the others are second titles

    We need to pre-process this, by change the first * to #
    and the others to # + 1

    **** should be transformed to #####

- Cursive
    _first-class_

    This is the same as in Markdown

- Code Chunk
    [Day]

    This need to be changed to `Day`

-}


scanCoq :: String -> [TokenCoq]
scanCoq [] = []
scanCoq fileContent
  | isSimpleComment fileContent = let (v, r) = buildCommentElement SimpleComment fileContent "(* "
                                  in v : scanCoq r
  | isCoqDoc fileContent = let (v, r) = buildCommentElement CoqDoc fileContent "(** "
                           in v : scanCoq r
  | otherwise = let (_, token, v, r2) = consumeUntil ["(* ", "(** "] fileContent
                in CodeChunk v : scanCoq (token ++ r2)

isCoqDoc :: String -> Bool
isCoqDoc = L.isPrefixOf "(** "

isSimpleComment :: String -> Bool
isSimpleComment = L.isPrefixOf "(* "

--buildSimpleElement :: String -> String -> String -> (TokenCoq, String)
buildCommentElement f content tkStart
  = let (pre, rest1) = L.splitAt (length tkStart) content
        (_, _, mid, rest2) = consumeUntil [" *)", "*)"] rest1
    in (f mid, rest2)

