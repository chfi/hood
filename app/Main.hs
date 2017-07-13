{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ClassyPrelude hiding (take)

import Graphics.Vty.Attributes (defAttr)
import qualified Graphics.Vty as V
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Options.Applicative
import qualified Doom.WAD as WAD
import Doom.WAD.Types (Header(..), Directory, DirEntry(..), LumpData(..))
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Data.Word (Word16)
import qualified Brick.AttrMap as A
import Linear.V2 (V2(..))
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
import qualified Graphics.Doom as Gfx



data Options = Options { filename :: Text
                       , runGL :: Bool
                       }

optsParser :: ParserInfo Options
optsParser = info (helper <*> opts)
  $ fullDesc <>
    progDesc "Parse a WAD file" <>
    header "ShowDoom, a Doom WAD parser and explorer"
  where opts = Options
         <$> (pack <$>
              strArgument (metavar "FILENAME" <> help "path to WAD to parse"))
         <*> switch
              (long "opengl"
               <> help "start opengl window")


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
          parsed = WAD.parseWAD bs (toList dir)
          vs' = find (\x -> case x of
                         Right (VERTEXES _) -> True
                         _ -> False) parsed
          -- vs :: SVector (V2 Word16)
          -- vs = fromList [V2 1088 61856, V2 1024 61856, V2 1024 61888]
          -- vs = fromList [V2 1088 61856, V2 1024 61856, V2 1024 61888, V2 1088 61888, V2 1152 61888, V2 960 61888, V2 1280 61984, V2 832 61984, V2 1344 61984, V2 704 61984, V2 896 62144, V2 928 62144, V2 928 62176, V2 896 62176]
          vs = case vs' of
            Just (Right (VERTEXES x)) -> x
            _ -> fromList [V2 30000 30000, V2 (-2000) (-2000), V2 3000 0]

      _ <- when (runGL args) $ do
        putStrLn "Launching OpenGL"
        Gfx.runApp vs

      void $ M.defaultMain app initialState
