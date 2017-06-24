module Main where

import           Data.List
import           Data.Semigroup      ((<>))
import           Numeric
import           Options.Applicative
import           System.Environment
import           System.FilePath
import           System.IO

import           Scanner

data V2MdOptions
  = V2MdOptions { output :: Maybe String
                , source :: String
                }

pV2Md :: Parser V2MdOptions
pV2Md = V2MdOptions
  <$> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Write output to FILE"))
  <*> strArgument (metavar "SOURCE")

processOptions :: V2MdOptions -> IO ()
processOptions (V2MdOptions Nothing src)
  = processVFile (replaceExtension src "md") src
processOptions (V2MdOptions (Just output) src)
  = processVFile output src

processVFile :: String -> String -> IO()
processVFile output src
  = do content <- readFile src
       let res = scanCoq content
       print res

main :: IO ()
main = execParser opts >>= processOptions
  where
    opts = info (helper <*> pV2Md)
                (fullDesc <> progDesc "Generate Pandoc files from Coq (.v)"
                          <> header "v2md (Coq to Pandoc)")
