module Scanner where

import qualified Data.List as L

data TokenCoq
  = SimpleComment String
  | CoqDoc String -- [DocElem]
  | CodeChunk String
  deriving Show

data DocElem
  = DocTextBlock [DocLine]
  | DocCodeBlock [DocLine]
  | DocListBlock [DocLine]
  deriving Show

data DocLine
  = DocLine String
  deriving Show

scanCoq :: String -> [TokenCoq]
scanCoq [] = []
scanCoq fileContent
  | isSimpleComment fileContent = let (v, r) = buildCommentElement SimpleComment fileContent "(* "
                                  in v : scanCoq r
  | isCoqDoc fileContent = let (v, r) = buildCommentElement CoqDoc fileContent "(** "
                           in v : scanCoq r
  | otherwise = let (r1, v, r2) = consumeUntil ["(* ", "(** "] fileContent
                in CodeChunk v : scanCoq (r1 ++ r2)

isCoqDoc :: String -> Bool
isCoqDoc = L.isPrefixOf "(** "

isSimpleComment :: String -> Bool
isSimpleComment = L.isPrefixOf "(* "

--buildSimpleElement :: String -> String -> String -> (TokenCoq, String)
buildCommentElement f content tkStart
  = let (pre, rest1) = L.splitAt (length tkStart) content
        (_, mid, rest2) = consumeUntil [" *)", "*)"] rest1
    in (f mid, rest2)

consumeUntil :: [String] -> String -> (String, String, String)
consumeUntil = consumeUntilWithRest []
  where consumeUntilWithRest middle tokens []
            = ([], middle, []) --error $ "End token '" ++ show tokens ++ "' not found."
        consumeUntilWithRest middle tokens content
            = case isPrefixTokenOf tokens content of
                Nothing -> let (r1, r2) = L.splitAt 1 content
                           in consumeUntilWithRest (middle ++ r1) tokens r2
                Just tk -> let (pre, restContent) = L.splitAt (length tk) content
                           in (pre, middle, restContent)
        isPrefixTokenOf :: [String] -> String -> Maybe String
        isPrefixTokenOf [] _ = Nothing
        isPrefixTokenOf (tk: tkList) cnt = if L.isPrefixOf tk cnt
                                           then Just tk
                                           else isPrefixTokenOf tkList cnt
