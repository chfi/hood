module Doom.UI.Lump
       where

import ClassyPrelude

import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Image (text', string, vertCat)
import qualified Graphics.Vty as V

import Doom.WAD.Types (Header(..), Directory, DirEntry(..), LumpData(..))
import qualified Doom.WAD as WAD

import Control.Lens
import Numeric (showHex)
import Data.Text (chunksOf, strip)
import Data.List (unfoldr)

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
-- import qualified Data.Vector as Vec
import Brick.Types
  ( Widget(..)
  , Size(..)
  , getContext
  , attrL
  , availWidthL
  , availHeightL
  , Result(..)
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , txt
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import qualified Brick.Widgets.Core as BC
import Brick.Util (fg, on)

-- customAttr :: A.AttrName
-- customAttr = L.listSelectedAttr <> "custom"


-- Draw lump information (name + size)
lumpInfo :: Maybe DirEntry -> Widget ()
lumpInfo mde = info
  where info = BC.padLeft (T.Pad 4) (nameW <=> sizeW <=> posW)
        lumpName = maybe "N/A" name mde
        lumpSize = maybe "N/A" (tshow . size) mde
        lumpPos  = maybe (const "N/A") (showHex . entryPtr) mde
        nameW = txt $ "Lump: " <> lumpName
        sizeW = txt $ "Size: " <> lumpSize <> " bytes"
        posW  = txt $ "Pos:  0x" <> (pack $ lumpPos "")


showHex' :: (Show t, Integral t) => t -> String
showHex' x = if length hex < 2 then "0" <> hex else hex
  where hex = showHex x ""

bstohex :: ByteString -> [Text]
bstohex bs = (ofoldMap (\x -> [pack $ showHex' x])) $ toByteVector bs

drawData :: LumpData -> Widget ()
drawData (Verbatim bs) = BC.padLeft (T.Pad 1) $ Widget Greedy Greedy $ do
  ctx <- getContext
  let a = ctx ^. attrL
      w = ctx ^. availWidthL
      h = ctx ^. availHeightL
      lineW = w `div` 3
      ls = unfoldr (\chunks ->
                      let (line , rest) = splitAt lineW chunks
                      -- in Just (text' a $ concat line , rest)) $ bstohex bs
                      in Just (text' a $ intercalate " " line , rest)) $ bstohex bs
  return $ Result (vertCat (take h ls)) [] [] []



-- Preview lump (if applicable)
-- should really be Maybe Lump -> Widget ();
-- i.e. the Lump is the data itself

lumpPreview :: ByteString -> Maybe DirEntry -> Widget ()
lumpPreview bs mde = prev
  where prev = B.borderWithLabel (txt "Lump preview") $ C.center lumpData
        -- lumpData = txt $ tshow $ concat $ take 1000 $ repeat "0"
        lumpData = drawData $ lump
        lump = case mde of
          Nothing -> Verbatim ""
          Just de -> WAD.indexLump bs de

-- Putting it together
-- drawUI :: L.List () DirEntry -> [Widget ()]
drawUI :: ByteString -> Maybe DirEntry -> Widget ()
drawUI bs mde = ui
  where ui = B.border $ lumpInfo mde <=> lumpPreview bs mde



-- bstohex :: ByteString -> SVector String
-- bstohex = (fmap (showHex)) . toByteVector


handleEvent :: L.List () DirEntry -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () DirEntry))
handleEvent l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    ev -> M.continue =<< L.handleListEvent ev l
handleEvent l _ = M.continue l
