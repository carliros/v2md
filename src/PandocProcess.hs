module PandocProcess where

import qualified Data.List as L
import qualified Data.Text as T
import Data.String.Conversions

import Scanner
import Utils

data TokenMd
  = EmptyMd
  | CoqMd String
  | CodeMd String

instance Show TokenMd where
  showsPrec _ (EmptyMd) = showString ""
  showsPrec _ (CoqMd str) = showString $ str ++ "\n"
  showsPrec _ (CodeMd str) = showString ("````\n" ++ str ++ "\n````\n")

genMarkdown :: String -> String
genMarkdown content
  = let scanResult = scanCoq content
        strippedCnt = map myStrip scanResult
        noEmptyCnt = filter isEmptyCnt strippedCnt
        genResult = processTokens noEmptyCnt
    in L.unlines $ map show genResult


processTokens :: [TokenCoq] -> [TokenMd]
processTokens [] = []
processTokens (SimpleComment _ :xs) = EmptyMd : processTokens xs
processTokens (CodeChunk c :xs) = CodeMd c : processTokens xs
processTokens (CoqDoc inp : xs) = processForPandoc inp : processTokens xs

processForPandoc :: String -> TokenMd
processForPandoc codeDoc = let cntMd = makePandoc codeDoc
                           in CoqMd (L.concat cntMd)

makePandoc :: String -> [String]
makePandoc [] = []
makePandoc codeDoc
  | isTitle codeDoc = let (title, rest) = makeTitle codeDoc
                      in title : makePandoc rest
  | isCodeChunk codeDoc = let (_, _, middle, rest) = consumeUntil ["]"] (tail codeDoc)
                              codeChunk = "`" ++ middle ++ "`"
                          in codeChunk : makePandoc rest
  | otherwise       = take 1 codeDoc : makePandoc (tail codeDoc)


isTitle str = let (ast, rest) = L.span ('*' ==) str
              in (length ast > 0) && (not (L.null rest) && head rest == ' ')

makeTitle str = let (ast, rest) = L.span ('*' ==) str
                in (L.replicate (length ast) '#', rest)

isCodeChunk (x:xs) = (x == '[') && (endsWithEndSquare xs)
  where endsWithEndSquare str = let (bool, _, _, _) = consumeUntil ["]"] str
                                in bool

myStrip (SimpleComment str) = SimpleComment $ cs (T.strip $ cs str)
myStrip (CoqDoc str) = let newStr = removeTrailingSpacesInLines str
                       in CoqDoc $ cs (T.strip $ cs newStr)
myStrip (CodeChunk str) = CodeChunk $ cs (T.strip $ cs str)

isEmptyCnt (SimpleComment str) = not $ L.null str
isEmptyCnt (CoqDoc str) = not $ L.null str
isEmptyCnt (CodeChunk str) = not $ L.null str

removeTrailingSpacesInLines [] = []
removeTrailingSpacesInLines str
  = let (bool, token, middle, rest) = consumeUntil ["\n\r    ", "\n    "] str
    in if bool
        then middle ++ "\n" ++ removeTrailingSpacesInLines rest
        else middle ++ removeTrailingSpacesInLines rest