module Utils where

import qualified Data.List as L

consumeUntilWithException :: [(String, [String])] -> String -> (Bool, String, String, String)
consumeUntilWithException = consumeUntilWithRest []
  where consumeUntilWithRest middle tokens []
            = (False, [], middle, [])
        consumeUntilWithRest middle tokens content
            = case isPrefixTokenOf tokens content of
                Nothing -> let (r1, r2) = L.splitAt 1 content
                           in consumeUntilWithRest (middle ++ r1) tokens r2
                Just tk -> let (token, restContent) = L.splitAt (length tk) content
                           in (True, token, middle, restContent)
        isPrefixTokenOf :: [(String, [String])] -> String -> Maybe String
        isPrefixTokenOf [] _ = Nothing
        isPrefixTokenOf ((tk, exceptions): tkList) cnt
            = if L.isPrefixOf tk cnt
              then if L.null exceptions
                   then Just tk
                   else if isPrefixListException cnt exceptions
                        then Just tk
                        else isPrefixTokenOf tkList cnt
              else isPrefixTokenOf tkList cnt
        isPrefixListException content = L.or . L.map (\tk -> L.isPrefixOf tk content)


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
