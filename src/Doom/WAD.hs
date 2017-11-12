module Doom.WAD where

import ClassyPrelude hiding (take)
-- import Data.Attoparsec.ByteString.Lazy (Parser, parseOnly, many, endOfInput)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Data.Word (Word16, Word32)
import Data.Int (Int16)
import Linear.V2 (V2(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary as Bin
import qualified Data.Text as T
import qualified Data.List as List
-- import Data.Text.Encoding (decodeUtf8)

import qualified Data.Vector as Vec

import Control.Error (hush)


import Doom.WAD.Types
import Doom.Map
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

parseLinedef :: Parser (Linedef Word16 Word16)
parseLinedef = do
  sv <- parseWord16
  ev <- parseWord16
  flags <- parseWord16
  parseWord16
  parseWord16
  rsd <- parseWord16
  lsd <- parseWord16
  pure $ Linedef sv ev flags rsd lsd


parseSidedef :: Parser (Sidedef Word16)
parseSidedef = do
  x <- parseWord16
  y <- parseWord16
  ut <- (decodeUtf8 <$> take 8)
  lt <- (decodeUtf8 <$> take 8)
  mt <- (decodeUtf8 <$> take 8)
  sec <- parseWord16
  pure $ Sidedef x y ut lt mt sec


parseSector :: Parser Sector
parseSector = do
  fh <- parseWord16
  ch <- parseWord16
  ft <- (decodeUtf8 <$> take 8)
  ct <- (decodeUtf8 <$> take 8)
  ll <- parseWord16
  t <- parseWord16
  parseWord16
  pure $ Sector fh ch ft ct ll t


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

dropTilMap :: [DirEntry] -> [DirEntry]
dropTilMap dir = dropWhile (\de -> not $ isMap $ name de) dir
  where isMap x = isDoom1 x || isDoom2 x
        isDoom1 x = (T.toLower x `index` 0 == Just 'e' &&)
                    (T.toLower x `index` 2 == Just 'm')
        isDoom2 x = "map" `T.isPrefixOf` T.toLower x


-- test = (!)

parseRawMap :: ByteString -> [DirEntry] -> Maybe (RawMap, [DirEntry])
parseRawMap wad dir = do
  title <- index dir 0
  linedefs <- index dir 2
  sidedefs <- index dir 3
  vertexes <- index dir 4
  sectors <- index dir 8

  let getLump = lumpSubstring wad
      parse ps = many ps <* endOfInput

  parsedVs <- hush $ parseOnly (parse parseVertex) (getLump vertexes)
  parsedLds <- hush $ parseOnly (parse parseLinedef) (getLump linedefs)
  parsedSds <- hush $ parseOnly (parse parseSidedef) (getLump sidedefs)
  parsedSecs <- hush $ parseOnly (parse parseSector) (getLump sectors)

  let rawMap = RawMap (name title) (Vec.fromList parsedVs)
                                   (Vec.fromList parsedLds)
                                   (Vec.fromList parsedSds)
                                   (Vec.fromList parsedSecs)

  pure $ (rawMap, drop 11 dir)

parseMaps :: ByteString -> [DirEntry] -> [RawMap]
parseMaps wad de = List.unfoldr (parseRawMap wad) (dropTilMap de)
