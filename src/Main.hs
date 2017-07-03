
module Main where

import ClassyPrelude hiding (take)

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
-- import Data.Text


-- see https://zdoom.org/wiki/WAD
-- and http://doom.wikia.com/wiki/WAD

data WADType = IWAD | PWAD deriving (Eq, Show, Read)

-- data Header = Header WADType Int Int
data Header = Header { wadtype :: WADType
                     , numLumps :: Int
                     , dirPtr :: Int
                     }


parse32BitInt :: Parser Int
parse32BitInt = C.readInt <$> take 4 >>= maybe mzero (pure . fst)

parseHeader :: Parser Header
parseHeader = do
  -- 0x00-0x03: "IWAD" or "PWAD"
  wadtype <- (string "IWAD" >> pure IWAD) <|> (string "PWAD" >> pure PWAD)
  -- 0x04-0x07: number of lumps
  numLumps <- parse32BitInt
  -- 0x08-0x0b: pointer to directory
  dirPtr <- parse32BitInt

  pure $ Header wadtype numLumps dirPtr


data DirEntry = DirEntry { entryPtr :: Int
                         , size :: Int
                         , name :: Text
                         }


-- parseDirEntry :: Parser DirEntry
-- parseDirEntry = do



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
  putStrLn "hello world"
