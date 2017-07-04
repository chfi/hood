{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ClassyPrelude hiding (take)

import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Options.Applicative (Parser, strOption, long, help)

import qualified Doom.WAD as WAD
import Doom.WAD.Types (Header(..))

data Options = Options { filename :: Text }

optsParser :: Parser Options
optsParser = Options
  <$> (pack <$> strOption (long "filename" <> help "path to WAD to parse"))


main :: IO ()
main = do
  -- opts <- execParser optsParser
  args <- getArgs
  case args of
    [] -> putStrLn "No file given"
    (name:_) -> do
      putStrLn $ "reading file: " <> name
      bs <- readFile $ unpack name
      case parseOnly WAD.parseHeader bs of
        Left err -> putStrLn $ pack err
        Right hd -> do
          print hd
          let dirBS = drop (fromIntegral $ dirPtr hd) bs
          case parseOnly WAD.parseDirectory dirBS of
            Left err -> putStrLn $ pack err
            Right dir -> do
              print dir
              print $ length dir
              print $ numLumps hd
