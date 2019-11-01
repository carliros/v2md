module Scanner where

import qualified Data.List as L
import qualified Data.Char as C
import Utils

data TokenCoq
  = SimpleComment String
  | CoqDoc String
  | CodeChunk String
  deriving Show

scanCoq :: String -> [TokenCoq]
scanCoq [] = []
scanCoq fileContent@(x:xs)
  -- | C.isSpace x = let (_, rest) = L.span C.isSpace fileContent
  --                 in scanCoq rest
  | isSimpleComment fileContent
      = let (v, r) = buildCommentElement SimpleComment fileContent "(* "
        in v : scanCoq r
  | isCoqDoc fileContent
      = let (v, r) = buildCommentElement CoqDoc fileContent "(** "
        in v : scanCoq r
  | otherwise
      = let (_, token, v, r2) = consumeUntilWithException [("(* ", ["(* #######", "(* ======="]), ("(** ", [])] fileContent
        in CodeChunk v : scanCoq (token ++ r2)

isCoqDoc :: String -> Bool
isCoqDoc = L.isPrefixOf "(** "

isSimpleComment :: String -> Bool
isSimpleComment content = L.or $ L.map (\tk -> L.isPrefixOf tk content) ["(* ", "(* #", "(* ="]

buildCommentElement :: (String -> TokenCoq) -> String -> String -> (TokenCoq, String)
buildCommentElement constructor input' tkStart
  = let (_, input) = L.splitAt (length tkStart) input'
        (_, _, content, inputLeft) = consumeUntil [" *)", "*)"] input
    in (constructor content, inputLeft)
