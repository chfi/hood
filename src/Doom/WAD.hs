module Doom.WAD where

import ClassyPrelude hiding (take)

import Data.Attoparsec.ByteString.Lazy
import Data.Word (Word16, Word32)
import Linear.V2 (V2(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary as Bin

import Doom.WAD.Types (WADType(..), Header(..), DirEntry(..), LumpData(..), Directory, Vertex)
-- see https://zdoom.org/wiki/WAD
-- and http://doom.wikia.com/wiki/WAD


parseWord32 :: Parser Word32
parseWord32 = do
  bytes <- Bin.decodeOrFail . reverse . LBS.fromStrict <$> take 4
  case bytes of
    Left _ -> mzero
    Right (_,_,c)  -> pure c

parseWord16 :: Parser Word16
parseWord16 = do
  bytes <- Bin.decodeOrFail . reverse . LBS.fromStrict <$> take 2
  case bytes of
    Left _ -> mzero
    Right (_,_,c)  -> pure c

parseHeader :: Parser Header
parseHeader = Header
  <$> ((string "IWAD" >> pure IWAD) <|>
       (string "PWAD" >> pure PWAD)
        <?> "when reading WAD type")
  <*> (parseWord32 <?> "when reading number of lumps")
  <*> (parseWord32 <?> "when reading directory offset")


parseDirEntry :: Parser DirEntry
parseDirEntry = DirEntry
  <$> (parseWord32 <?> "when reading lump offset")
  <*> (parseWord32 <?> "when reading lump size")
  <*> (pack . C.unpack <$> take 8 <?> "when reading lump name")


parseDirectory :: Parser Directory
parseDirectory = fromList <$> many parseDirEntry <* endOfInput


-- | takes the whole WAD and a directory entry,
-- | returns a pair of the lump's name and its data as a bytestring
loadLump :: ByteString -> DirEntry -> (Text, ByteString)
loadLump bs d = (name d, BS.take (fromIntegral $ size d) $ BS.drop (fromIntegral $ entryPtr d) bs)

lumpSubstring :: ByteString -> DirEntry -> ByteString
lumpSubstring bs de =  BS.take (fromIntegral $ size de) $ drop (fromIntegral $ entryPtr de) bs

parseVertex :: Parser Vertex
parseVertex = V2 <$> parseWord16 <*> parseWord16

parseVerbatim :: Parser LumpData
parseVerbatim = Verbatim <$> takeByteString

-- parseTHINGS :: Parser LumpData
-- parseTHINGS = THINGS <$> takeByteString

parseVERTEXES :: Parser LumpData
parseVERTEXES = (VERTEXES . fromList) <$> many parseVertex <* endOfInput


getParser :: DirEntry -> Parser LumpData
getParser de = case name de of
  "VERTEXES" -> parseVERTEXES
  _ -> parseVerbatim


parseWAD :: ByteString -> [DirEntry] -> [Either String LumpData]
parseWAD bs dir = --fmap (parseOnly (getParser dir)) (lumpSubstring bs <$> dir)
  let f de = parseOnly (getParser de) (lumpSubstring bs de)
  in fmap f dir


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
