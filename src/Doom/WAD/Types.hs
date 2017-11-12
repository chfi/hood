module Doom.WAD.Types where

import ClassyPrelude
import Data.Int (Int16)
import Data.Word (Word16)
import Linear.V2 (V2(..))

data WADType = IWAD | PWAD deriving (Eq, Show, Read)


-- TODO: use Ptr for dirPtr and entryPtr instead?
-- would that make indexing easier?
-- actually, can I just read it as a C struct?
-- well, still doesn't make sense to make it a ptr, I guess
data Header = Header { wadtype :: WADType
                     , numLumps :: Word32
                     , dirPtr :: Word32
                     } deriving (Eq, Show)


type Vertex = V2 Int16

-- TODO there's more to linedefs than this
-- data Linedef = Linedef { startVertex :: Word16
--                        , endVertex :: Word16
--                        } deriving (Eq, Ord, Show)

data Linedef v sd = Linedef { startVertex :: v
                            , endVertex :: v
                            , flags :: Word16
                            , rightSidedef :: sd
                            , leftSidedef :: sd
                            } deriving (Eq, Ord, Show)

data Sidedef sec = Sidedef { xOffset :: Word16
                           , yOffset :: Word16
                           , upperTexture :: Text
                           , lowerTexture :: Text
                           , middleTexture :: Text
                           , sector :: sec
                           } deriving (Eq, Ord, Show)

data Sector = Sector { floorHeight :: Word16
                     , ceilingHeight :: Word16
                     , floorTexture :: Text
                     , ceilingTexture :: Text
                     , lightLevel :: Word16
                     , sectorType :: Word16
                     } deriving (Eq, Ord, Show)

data LumpData  = Verbatim ByteString
               | Map ByteString
               | THINGS ByteString
               | LINEDEFS (Vector (Linedef Word16 Word16))
               | SIDEDEFS ByteString
               | VERTEXES (SVector Vertex)
               | SEGS ByteString
               | SUBSECTORS ByteString
               | NODES ByteString
               | SECTORS ByteString
               | REJECT ByteString
               | BLOCKMAP ByteString
               -- | FLATS ByteString
               -- | SPRITES ByteString
               -- | PATCHES ByteString
               -- | PLAYPAL ByteString
               -- | COLORMAP ByteString
               -- | ENDOOM ByteString
               -- | TEXTURE1 ByteString
               -- | TEXTURE2 ByteString
               -- | PNAMES ByteString
               -- | DEMO ByteString
               deriving (Eq, Ord, Show)


data DirEntry = DirEntry { entryPtr :: Word32
                         , size :: Word32
                         , name :: Text
                         } deriving (Eq, Show)

type Directory = Vector DirEntry





-- data DoomMap = DoomMap { title :: Text
--                        , vertexes :: SVector Vertex
--                        , linedefs :: Vector Linedef
--                        } deriving (Eq, Ord, Show)
