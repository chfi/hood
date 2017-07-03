{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ClassyPrelude hiding (take)

import Data.Attoparsec.ByteString.Lazy
import Data.Word (Word32)
-- import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C
import Data.Semigroup ((<>))
import Options.Applicative hiding (Parser)
import qualified Data.Binary as Bin
-- import Data.Text


-- see https://zdoom.org/wiki/WAD
-- and http://doom.wikia.com/wiki/WAD

data WADType = IWAD | PWAD deriving (Eq, Show, Read)

-- data Header = Header WADType Int Int
data Header = Header { wadtype :: WADType
                     , numLumps :: Int
                     , dirPtr :: Word32
                     } deriving (Eq, Show)


-- decodeLil
-- decodeLil = Bin.decodeOrFail . reverse


parse32BitInt :: Parser Int
parse32BitInt = (readMay . show) <$> take 4 >>= maybe mzero pure

parseWord32 :: Parser Word32
parseWord32 = do
  bytes <- Bin.decodeOrFail . reverse . LBS.fromStrict <$> take 4
  case bytes of
    Left err -> mzero
    Right (_,_,c)  -> pure c

parseHeader :: Parser Header
parseHeader = do
  -- 0x00-0x03: "IWAD" or "PWAD"
  wadtype <- (string "IWAD" >> pure IWAD) <|> (string "PWAD" >> pure PWAD)
  -- 0x04-0x07: number of lumps
  numLumps <- parseWord32

  -- numLumps <- show <$> take 4
  -- 0x08-0x0b: pointer to directory
  dirPtr <- parseWord32

  -- pure $ Header wadtype numLumps dirPtr
  pure $ Header wadtype (fromIntegral numLumps) (fromIntegral dirPtr)



data DirEntry = DirEntry { entryPtr :: Word32
                         , size :: Word32
                         , name :: Text
                         } deriving (Eq, Show)


parseDirEntry :: Parser DirEntry
parseDirEntry = do
  entryPtr <- parseWord32
  size <- parseWord32
  name <- pack . C.unpack <$> take 8
  pure $ DirEntry entryPtr size name



type Directory = [DirEntry]



parseDirectory :: Parser Directory
parseDirectory = (many parseDirEntry) <* endOfInput


{- TODO
read Header
print Header
read Directory & populate list of DirEntrys
Create UI for scrolling through the list of DirEntrys
Then go for all the special cases:
+ Maps
+ Flats
+ Sprites
+ Patches
Specific names:
+ PLAYPAL
+ COLORMAP
+ ENDOOM
+ TEXTURE1, TEXTURE2, PNAMES


Perhaps with calling out to other programs to open and view them?
(That's when we'd go for an actual GUI)
(Or just open an OpenGL window... hm)

Maybe editing text?!
-}




main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No file given"
    (name:_) -> do
      putStrLn $ "reading file: " <> name
      bs <- readFile $ unpack name
      case parseOnly parseHeader bs of
        Left err -> putStrLn $ pack err
        Right hd -> do
          print hd
          let dirBS = drop (fromIntegral $ dirPtr hd) bs
          case parseOnly parseDirectory dirBS of
            Left err -> putStrLn $ pack err
            Right dir -> do
              print dir
              print $ length dir
              print $ numLumps hd
