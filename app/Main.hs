{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ClassyPrelude hiding (take)

import Graphics.Vty.Attributes (defAttr)
import qualified Graphics.Vty as V
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Options.Applicative
import qualified Doom.WAD as WAD
import Doom.WAD.Types (Header(..), Directory, DirEntry(..))
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


-- appDraw :: ByteString -> L.List () DirEntry -> [Widget ()]
appDraw :: ByteString -> AppState -> [Widget ()]
appDraw bs (_,l) = [ui]
  where ui = (UI.Directory.drawUI l) <+> (UI.Lump.drawUI bs de)
        de = snd <$> L.listSelectedElement l


-- appEvent :: L.List () DirEntry -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () DirEntry))
appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent as@(m,l) (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt as
    -- handle directory list events
    ev -> do
      l' <- L.handleListEvent ev l
      M.continue (m,l')
appEvent as _ = M.continue as


data Mode = BytesPreview | ParsePreview

type AppState = (Mode, L.List () DirEntry)


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

      let app = M.App { M.appDraw = appDraw bs
                      , M.appChooseCursor = M.showFirstCursor
                      , M.appHandleEvent = appEvent
                      , M.appStartEvent = return
                      , M.appAttrMap = const $ A.attrMap defAttr []
                      }
          initialState = (BytesPreview, L.list () dir 1)

      void $ M.defaultMain app initialState
