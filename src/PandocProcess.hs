module PandocProcess where

import qualified Data.List as L
import qualified Data.Text as T
import Data.String.Conversions

import Scanner
import Utils

data TokenMd
  = EmptyMd
  | CoqComment String
  | CoqMd String
  | CodeMd String

instance Show TokenMd where
  showsPrec _ (EmptyMd) = showString ""
  showsPrec _ (CoqComment str) = showString ""
  showsPrec _ (CoqMd str) = showString $ str ++ "\n"
  showsPrec _ (CodeMd str) = showString ("````coq\n" ++ str ++ "\n````\n")

genMarkdown :: String -> String
genMarkdown = L.unlines
            . map show
            . processTokens
            . filter isEmptyContent -- discard empty tokens
            . map myStrip
            . scanCoq

processTokens :: [TokenCoq] -> [TokenMd]
processTokens [] = []
processTokens (SimpleComment c : xs)
  | recognizeSectionSymbolComment '#' c
      = processTokens (increaseNextTitleLeveBy 1 xs)
  | recognizeSectionSymbolComment '=' c
      = processTokens (increaseNextTitleLeveBy 1 xs)
  | otherwise = CoqComment c : processTokens xs
processTokens (CodeChunk c :xs) = CodeMd c : processTokens xs
processTokens (CoqDoc inp : xs) = processForPandoc inp : processTokens xs

increaseNextTitleLeveBy :: Int -> [TokenCoq] -> [TokenCoq]
increaseNextTitleLeveBy _ [] = []
increaseNextTitleLeveBy n (x:xs) = increaseTitle x : xs
  where increaseTitle (CoqDoc title)
            = if L.isPrefixOf "*" title
              then CoqDoc $ (L.replicate n '*') ++ title
              else CoqDoc title
        increaseTitle i = i

recognizeSectionSymbolComment :: Char -> String -> Bool
recognizeSectionSymbolComment symbol content
  = let (section, rest) = L.span (symbol ==) content
    in L.length section > 10

-- | 'processForPandoc' recognize and convert to pandoc
processForPandoc :: String -> TokenMd
processForPandoc = CoqMd . L.concat . makePandoc

-- | 'makePandoc' builds a pandoc string
makePandoc :: String -> [String]
makePandoc [] = []
makePandoc codeDoc
  | isTitle codeDoc
      = let (title, rest) = makeTitle codeDoc
        in title : makePandoc rest
  | isCodeChunk codeDoc
      = let (_, _, middle, rest) = consumeUntil ["]"] (tail codeDoc)
            codeChunk = "`" ++ middle ++ "`"
        in codeChunk : makePandoc rest
  | otherwise
      = take 1 codeDoc : makePandoc (tail codeDoc)

-- | 'isTitle' checks if the string is any kind of coq title
isTitle :: String -> Bool
isTitle str = let (ast, rest) = L.span ('*' ==) str
              in (length ast > 0) && (not (L.null rest) && head rest == ' ')

-- | 'makeTitle' make a md title from coq title
makeTitle :: String -> (String, String)
makeTitle str = let (ast, rest) = L.span ('*' ==) str
                in (L.replicate (length ast) '#', rest)

-- | 'isCodeChunk' checks a chunk of coq code
isCodeChunk (x:xs) = (x == '[') && (endsWithEndSquare xs)
  where endsWithEndSquare str = let (bool, _, _, _) = consumeUntil ["]"] str
                                in bool

-- | 'myStrip' removes trailing spaces between lines
myStrip (SimpleComment str) = SimpleComment $ cs (T.strip $ cs str)
myStrip (CoqDoc str) = let newStr = removeTrailingSpacesInLines str
                       in CoqDoc $ cs (T.strip $ cs newStr)
myStrip (CodeChunk str) = CodeChunk $ cs (T.strip $ cs str)

-- | 'isEmptyContent' returns True if the string content is empty
isEmptyContent (SimpleComment str) = not $ L.null str
isEmptyContent (CoqDoc str) = not $ L.null str
isEmptyContent (CodeChunk str) = not $ L.null str

-- | 'removeTrailingSpacesInLines' for coq doc comments.
-- | TODO: needs love here. Consume until ["\n\r    ", "\n    "] is too hacking
removeTrailingSpacesInLines [] = []
removeTrailingSpacesInLines str
  = let (bool, token, middle, rest) = consumeUntil ["\n\r    ", "\n    "] str
    in if bool
        then middle ++ "\n" ++ removeTrailingSpacesInLines rest
        else middle ++ removeTrailingSpacesInLines rest
