module Doom.UI.Directory
       ( drawUI
       , handleEvent
       ) where

import ClassyPrelude hiding (take)

import Graphics.Vty.Attributes (defAttr)
import qualified Graphics.Vty as V

import Doom.WAD.Types (Header(..), Directory, DirEntry(..))

import Control.Lens
import Numeric (showHex)

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
-- import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
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

drawEntryName :: Bool -> DirEntry -> Widget ()
drawEntryName sel d =
  let selStr s = if sel
                 then (txt $ "<" <> s <> ">")
                 else txt s
  -- in C.hCenter $ (selStr $ name d)
  -- in hLimit 12 $ C.hCenter (selStr $ name d)
  in BC.padLeft (T.Pad 2) (selStr $ name d)

drawUI :: L.List () DirEntry -> Widget ()
drawUI l = ui
  where ui = B.border $ hLimit 13 $ L.renderList drawEntryName True l


-- bstohex :: ByteString -> SVector String
-- bstohex = (fmap (showHex)) . toByteVector


handleEvent :: L.List () DirEntry -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () DirEntry))
handleEvent l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    ev -> M.continue =<< L.handleListEvent ev l
handleEvent l _ = M.continue l
