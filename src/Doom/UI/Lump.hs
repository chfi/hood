module Doom.UI.Lump
       where

import ClassyPrelude

import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Image (text', string)
import qualified Graphics.Vty as V

import Doom.WAD.Types (Header(..), Directory, DirEntry(..))

import Control.Lens
import Numeric (showHex)
import Data.Text (chunksOf)

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


bstohex :: ByteString -> Text
bstohex bs = (ofoldMap (\x -> pack $ (showHex x) " ")) $ toByteVector bs

drawData :: ByteString -> Widget ()
drawData bs = Widget Greedy Greedy $ do
  ctx <- getContext
  let a = ctx ^. attrL
      w = ctx ^. availWidthL
      h = ctx ^. availHeightL
      -- break the hex into `w` long lines
      hex = chunksOf w $ bstohex bs
  return $ Result (text' a hex) [] [] []



-- Preview lump (if applicable)
-- should really be Maybe Lump -> Widget ();
-- i.e. the Lump is the data itself
lumpPreview :: Maybe DirEntry -> Widget ()
lumpPreview mde = prev
  where prev = B.borderWithLabel (txt "Lump preview") $ C.center lumpData
        -- lumpData = txt $ tshow $ concat $ take 1000 $ repeat "0"
        lumpData = drawData (concat $ take 1000 $ repeat "0")

-- Putting it together
-- drawUI :: L.List () DirEntry -> [Widget ()]
drawUI :: Maybe DirEntry -> Widget ()
drawUI mde = ui
  where ui = B.border $ lumpInfo mde <=> lumpPreview mde



-- bstohex :: ByteString -> SVector String
-- bstohex = (fmap (showHex)) . toByteVector


handleEvent :: L.List () DirEntry -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () DirEntry))
handleEvent l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    ev -> M.continue =<< L.handleListEvent ev l
handleEvent l _ = M.continue l
