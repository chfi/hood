module Graphics.Camera where

import ClassyPrelude

import Graphics.GL (GLfloat)

import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.Matrix (M44)

import Linear (lookAt, perspective)

import Graphics.UI.GLFW (Window, Key, KeyState, ModifierKeys)
import qualified Graphics.UI.GLFW as GLFW



data Camera = Camera
  { _pos :: V3 GLfloat
  , _camF :: V3 GLfloat
  , _camU :: V3 GLfloat
  } deriving (Eq, Ord, Show)


data CameraDelta = CameraDelta
  { _posD :: V3 GLfloat
  , _hAngD :: GLfloat
  , _vAngD :: GLfloat
  } deriving (Eq, Ord, Show)




mainKeyCallback :: IORef PlayerState -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
mainKeyCallback psref w k i ks mk = do
  let del = if ks == GLFW.KeyState'Pressed ||
               ks == GLFW.KeyState'Repeating then 1.0 else 0.0
  case k of
    GLFW.Key'Escape -> GLFW.setWindowShouldClose w True
    GLFW.Key'Left -> do
      modifyIORef psref $ \ps ->
        let c = cross (_camF ps) (_camU ps)
        in ps & posD .~ c * (-0.05) * pure del
    GLFW.Key'Right -> do
      modifyIORef psref $ \ps ->
        let c = cross (_camF ps) (_camU ps)
        in ps & posD .~ c *   0.05  * pure del
    GLFW.Key'Up -> do
      modifyIORef psref $ \ps ->
        ps & posD .~ view camF ps * pure del *   0.1
    GLFW.Key'Down -> do
      modifyIORef psref $ \ps ->
        ps & posD .~ view camF ps * pure del * (-0.1)
    _ -> pure ()

{-
mainCursorCallback :: IORef (Double, Double)
                   -> IORef PlayerState -> Window -> Double -> Double -> IO ()
mainCursorCallback mref psref _ x y = do
  (lx,ly) <- readIORef mref
  let dx = x - lx
      dy = y - ly
  writeIORef mref (x, y)
  modifyIORef psref $ hAngD .~ double2Float dx * (-0.01)
  modifyIORef psref $ vAngD .~ double2Float dy * (-0.01)
-}


createMatrix :: Camera -> M44 GLfloat
createMatrix Camera{..} = lookAt _pos (_pos + _camF) _camU
