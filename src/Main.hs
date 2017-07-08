{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ClassyPrelude hiding (take)

import Graphics.Vty.Attributes (defAttr)
import qualified Graphics.Vty as V
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Options.Applicative hiding (str)

import qualified Doom.WAD as WAD
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
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)


import qualified Doom.UI.Directory as UI.Directory
import qualified Doom.UI.Lump as UI.Lump

newtype Options = Options { filename :: Text }

optsParser :: ParserInfo Options
optsParser = info (helper <*> opts)
  $ fullDesc <>
    progDesc "Parse a WAD file" <>
    header "ShowDoom, a Doom WAD parser and explorer"
  where opts = Options
         <$> (pack <$>
              strArgument (metavar "FILENAME" <> help "path to WAD to parse"))


-- customAttr :: A.AttrName
-- customAttr = L.listSelectedAttr <> "custom"



appDraw :: L.List () DirEntry -> [Widget ()]
appDraw l = [ui]
  where ui = (UI.Directory.drawUI l) <+> (UI.Lump.drawUI de)
        de = snd <$> L.listSelectedElement l



appEvent :: L.List () DirEntry -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () DirEntry))
appEvent l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    -- handle directory list events
    ev -> M.continue =<< L.handleListEvent ev l
appEvent l _ = M.continue l



main :: IO ()
main = do
  args <- execParser optsParser
  putStrLn $ "reading file: " <> filename args
  bs <- readFile $ unpack (filename args)

  let result = do
        hd <- parseOnly WAD.parseHeader bs
        let dirBS = drop (fromIntegral $ dirPtr hd) bs
        dir <- parseOnly WAD.parseDirectory dirBS
        pure (hd, dir)

  case result of
    Left err -> print err
    Right (hd, dir) -> do


      let app = M.App { M.appDraw = appDraw
                      , M.appChooseCursor = M.showFirstCursor
                      , M.appHandleEvent = appEvent
                      , M.appStartEvent = return
                      , M.appAttrMap = const $ A.attrMap defAttr []
                      }
          initialState = L.list () dir 1

      void $ M.defaultMain app initialState
