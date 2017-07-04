module Doom.WAD where

import ClassyPrelude hiding (take)

import Data.Attoparsec.ByteString.Lazy
import Data.Word (Word32)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary as Bin

import Doom.WAD.Types (WADType(..), Header(..), DirEntry(..), Lump, Directory)
-- see https://zdoom.org/wiki/WAD
-- and http://doom.wikia.com/wiki/WAD


parse32BitInt :: Parser Int
parse32BitInt = (readMay . show) <$> take 4 >>= maybe mzero pure

parseWord32 :: Parser Word32
parseWord32 = do
  bytes <- Bin.decodeOrFail . reverse . LBS.fromStrict <$> take 4
  case bytes of
    Left _ -> mzero
    Right (_,_,c)  -> pure c

parseHeader :: Parser Header
parseHeader = Header
  -- 0x00-0x03: "IWAD" or "PWAD"
  <$> ((string "IWAD" >> pure IWAD) <|> (string "PWAD" >> pure PWAD))
  -- 0x04-0x07: number of lumps
  <*> parseWord32
  -- 0x08-0x0b: pointer to directory
  <*> parseWord32


parseDirEntry :: Parser DirEntry
parseDirEntry = DirEntry
  <$> parseWord32
  <*> parseWord32
  <*> (pack . C.unpack <$> take 8)


parseDirectory :: Parser Directory
parseDirectory = (many parseDirEntry) <* endOfInput


-- | takes the whole WAD and a directory entry,
-- | returns a pair of the lump's name and its data as a bytestring
loadLump :: ByteString -> DirEntry -> (Text, ByteString)
loadLump bs d = (name d, BS.take (fromIntegral $ size d) $ BS.drop (fromIntegral $ entryPtr d) bs)


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
