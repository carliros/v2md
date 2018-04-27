module Utils where

import qualified Data.List as L

consumeUntil :: [String] -> String -> (Bool, String, String, String)
consumeUntil = consumeUntilWithRest []
  where consumeUntilWithRest middle tokens []
            = (False, [], middle, []) --error $ "End token '" ++ show tokens ++ "' not found."
        consumeUntilWithRest middle tokens content
            = case isPrefixTokenOf tokens content of
                Nothing -> let (r1, r2) = L.splitAt 1 content
                           in consumeUntilWithRest (middle ++ r1) tokens r2
                Just tk -> let (token, restContent) = L.splitAt (length tk) content
                           in (True, token, middle, restContent)
        isPrefixTokenOf :: [String] -> String -> Maybe String
        isPrefixTokenOf [] _ = Nothing
        isPrefixTokenOf (tk: tkList) cnt = if L.isPrefixOf tk cnt
                                           then Just tk
                                           else isPrefixTokenOf tkList cnt
