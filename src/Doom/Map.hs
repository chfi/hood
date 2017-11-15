module Doom.Map where

import ClassyPrelude

import Data.Int (Int16)
import Data.Word (Word16)
import qualified Data.Vector as Vec
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
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


data RawMap = RawMap { rawName :: Text
                     , rawVertexes :: Vector Vertex
                     , rawLinedefs :: Vector (Linedef VertexNumber SidedefNumber)
                     , rawSidedefs :: Vector (Sidedef SectorNumber)
                     , rawSectors :: Vector Sector
                     } deriving (Eq, Ord, Show)


data DoomMap = DoomMap { mapName :: Text
                       , linedefs :: Vector (Linedef (V2 Float) (Maybe (Sidedef Sector)))
                       } deriving (Eq, Ord, Show)

buildSidedef :: Vector Sector -> Sidedef SectorNumber -> Maybe (Sidedef Sector)
buildSidedef secs sd@Sidedef{..} = case secs `index` fromIntegral sector of
  Nothing  -> Nothing
  Just sec -> Just sd { sector = sec }

buildLinedef :: Vector Vertex
             -> Vector (Sidedef Sector)
             -> Linedef VertexNumber SidedefNumber
             -> Maybe (Linedef (V2 Float) (Maybe (Sidedef Sector)))
buildLinedef verts sdefs ld@Linedef{..} = do
  sv <- verts `index` fromIntegral startVertex
  ev <- verts `index` fromIntegral endVertex
  let rsd = sdefs `index` fromIntegral rightSidedef
      lsd = sdefs `index` fromIntegral leftSidedef

  pure $ ld { startVertex = fromIntegral <$> sv
            , endVertex = fromIntegral <$> ev
            , rightSidedef = rsd
            , leftSidedef = lsd
            }


buildMap :: RawMap -> Maybe DoomMap
buildMap RawMap{..} = do
  sidedefs <- sequence $ Vec.map (buildSidedef rawSectors) rawSidedefs
  ldefs <- sequence $ Vec.map (buildLinedef rawVertexes sidedefs) rawLinedefs
  pure $ DoomMap rawName ldefs



getSectors' :: Vector (Linedef VertexNumber (Sidedef SectorNumber))
            -> Vector (Vector (VertexNumber, VertexNumber, SectorNumber))
getSectors' ldefs = Vec.fromList $ map (Vec.fromList) $ rs <> ls
  where ldefs' = toList ldefs
        rsides :: [(VertexNumber, VertexNumber, SectorNumber)]
        rsides = map (\Linedef{..} -> (startVertex, endVertex, sector rightSidedef)) ldefs'
        rs = groupBy (\(_,_,n1) (_,_,n2) -> n1 == n2) rsides
        lsides = map (\Linedef{..} -> (startVertex, endVertex, sector leftSidedef)) ldefs'
        ls = groupBy (\(_,_,n1) (_,_,n2) -> n1 == n2) lsides
  -- sort linedefs by sectornumber



buildSector :: Vector Vertex
            -> Vector Sector
            -> (Vector (VertexNumber, VertexNumber), SectorNumber)
            -> Maybe (Vector (V3 Float), Sector)
buildSector verts secs (vs, sn) = do
  let mkPlane :: Float -> (VertexNumber, VertexNumber) -> Vector (V3 Float)
      mkPlane z (v1, v2) = let (V2 x1 y1) = fromIntegral v1
                               (V2 x2 y2) = fromIntegral v2
                           in Vec.fromList [V3 x1 y1 z, V3 x2 y2 z]

  sector <- secs `index` fromIntegral sn
  let mkFloor :: (VertexNumber, VertexNumber) -> Vector (V3 Float)
      mkFloor = mkPlane (fromIntegral (floorHeight sector))
      mkCeil  = mkPlane (fromIntegral (ceilingHeight sector))


  pure ((mkFloor =<< vs) <> (mkCeil =<< vs)
       , sector)



-- group linedefs by same sector??
-- getSectors :: DoomMap -> Vector (Vector (V3 Float), Sector)
-- getSectors DoomMap{..} =
