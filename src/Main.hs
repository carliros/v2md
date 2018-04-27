module Main where

import           Data.List
import           Data.Semigroup      ((<>))
import           Numeric
import           Options.Applicative
import           System.Environment
import           System.FilePath
import           System.IO

import PandocProcess

data V2MdOptions
  = V2MdOptions { output :: Maybe String
                , source :: String
                }

pV2Md :: Parser V2MdOptions
pV2Md = V2MdOptions
  <$> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Write output to FILE"))
  <*> strArgument (metavar "SOURCE")

processOptions :: V2MdOptions -> IO ()
processOptions (V2MdOptions maybeOutput src)
  = processVFile maybeOutput src

processVFile :: Maybe String -> String -> IO()
processVFile maybeOutput src
  = do content <- readFile src
       let res = genMarkdown content
       case maybeOutput of
        Nothing -> putStr res
        Just fn -> writeFile fn res

main :: IO ()
main = execParser opts >>= processOptions
  where
    opts = info (helper <*> pV2Md)
                (fullDesc <> progDesc "Generate Pandoc files from Coq (.v)"
                          <> header "v2md (Coq to Pandoc)")
