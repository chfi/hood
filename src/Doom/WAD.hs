module Doom.WAD where

import ClassyPrelude hiding (take)
import Data.Attoparsec.ByteString.Lazy
import Data.Word (Word16, Word32)
import Data.Int (Int16)
import Linear.V2 (V2(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary as Bin

import Doom.WAD.Types
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

parseInt16 :: Parser Int16
parseInt16 = do
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

parseVerbatim :: Parser LumpData
parseVerbatim = Verbatim <$> takeByteString

parseVertex :: Parser Vertex
parseVertex = V2 <$> parseInt16 <*> parseInt16

parseLinedef :: Parser Linedef
parseLinedef = Linedef <$> parseWord16 <*> parseWord16
-- parseTHINGS :: Parser LumpData
-- parseTHINGS = THINGS <$> takeByteString

parseVERTEXES :: Parser LumpData
parseVERTEXES = (VERTEXES . fromList) <$> many parseVertex <* endOfInput


parseLINEDEFS :: Parser LumpData
parseLINEDEFS = (LINEDEFS . fromList) <$> many parseLinedef <* endOfInput


getParser :: DirEntry -> Parser LumpData
getParser de = case name de of
  "VERTEXES" -> parseVERTEXES
  "LINEDEFS" -> parseLINEDEFS
  _ -> parseVerbatim


parseWAD :: ByteString -> [DirEntry] -> [Either String LumpData]
parseWAD bs dir = --fmap (parseOnly (getParser dir)) (lumpSubstring bs <$> dir)
  let f de = parseOnly (getParser de) (lumpSubstring bs de)
  in fmap f dir

-- test = (!)

parseMap :: ByteString -> [DirEntry] -> Maybe (DoomMap, [DirEntry])
parseMap bs de = do
  title <- index de 0
  things <- index de 1
  linedefs <- index de 2
  sidedefs <- index de 3
  vertexes <- index de 4
  segs <- index de 5
  ssectors <- index de 6
  nodes <- index de 7
  sectors <- index de 8
  reject <- index de 9
  blockmap <- index de 10

  parsedVertexes <- case parseOnly (many parseVertex <* endOfInput) (lumpSubstring bs vertexes) of
    Left _ -> Nothing
    Right vs -> Just vs

  parsedLinedefs <- case parseOnly (many parseLinedef <* endOfInput) (lumpSubstring bs linedefs) of
    Left _ -> Nothing
    Right vs -> Just vs

  let dmap = DoomMap (name title) (fromList parsedVertexes) (fromList parsedLinedefs)
  pure (dmap, drop 11 de)

-- parseMaps :: ByteString -> [DirEntry] -> [DoomMap]
parseMaps :: ByteString -> [DirEntry] -> [DoomMap]
parseMaps bs des = case parseMap bs des of
  Nothing -> []
  Just (m,des') -> m:(parseMaps bs des')
-- lumpToMap :: [LumpData] -> M
-- lumpToMap


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
