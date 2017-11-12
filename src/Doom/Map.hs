module Doom.Map where

import ClassyPrelude

import Data.Int (Int16)
import Data.Word (Word16)
-- import Data.Vector ((!?))
import Linear.V2 (V2(..))
import Doom.WAD.Types


-- a DoomMap is a haskell representation of a Doom map from a WAD;
-- it's parsed and ready to render.


{-
a doom map contains:
a list of THINGS
a list of LINEDEFS
a list of SIDEDEFS
a list of VERTEXES
a list of SECTORS

and more, like:

a list of SEGS
a list of SUBSECTORS
a list of NODES
-}

type VertexNumber = Word16
type SidedefNumber = Word16
type SectorNumber = Word16

-- buildMap :: Vector

data RawMap = RawMap { rawName :: Text
                     , rawVertexes :: Vector Vertex
                     , rawLinedefs :: Vector (Linedef VertexNumber SidedefNumber)
                     , rawSidedefs :: Vector (Sidedef SectorNumber)
                     , rawSector :: Vector Sector
                     } deriving (Eq, Ord, Show)

-- type RawDoomMap = ( Vector (V2 Int16)
--                   , Vector (Linedef (V2 Word16) (Word16

data DoomMap = DoomMap { mapName :: Text
                       , linedefs :: Vector (Linedef (V2 Double) (Sidedef Sector))
                       } deriving (Eq, Ord, Show)

buildSidedef :: Vector Sector -> Sidedef SectorNumber -> Maybe (Sidedef Sector)
buildSidedef secs sd@Sidedef{..} = case secs `index` fromIntegral sector of
  Nothing  -> Nothing
  Just sec -> Just sd { sector = sec }

buildLinedef :: Vector Vertex
             -> Vector (Sidedef Sector)
             -> Linedef VertexNumber SidedefNumber
             -> Maybe (Linedef Vertex (Sidedef Sector))
buildLinedef verts sdefs ld@Linedef{..} = do
  sv <- verts `index` fromIntegral startVertex
  ev <- verts `index` fromIntegral endVertex
  rsd <- sdefs `index` fromIntegral rightSidedef
  lsd <- sdefs `index` fromIntegral leftSidedef
  pure $ ld { startVertex = sv
            , endVertex = ev
            , rightSidedef = rsd
            , leftSidedef = lsd
            }

-- buildLinedef :: Linedef WADOffset WADOffset -> Linedef

-- buildMap :: ByteString -> RawMap -> DoomMap
-- buildMap wad RawMap{..} = undefined
--   where name' = decodeUtf8 name
