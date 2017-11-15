module Graphics.Doom where

import ClassyPrelude


-----

import Control.Monad (when, forever)
import System.Exit (die, exitFailure, exitSuccess)

import Foreign hiding (rotate)

import Graphics.GL

import Graphics.Buffer (generateVertexArray, bindBuffer)
import Graphics.Camera
import Graphics.Shader (loadShaders)


import Linear.V2 (V2(..))
import Linear.V3 (V3(..))

import Control.Lens

import qualified Graphics.UI.GLFW as GLFW
import Doom.WAD.Types
import Doom.Map (DoomMap(..))


linedefLine :: V2 Float
            -> GLfloat
            -> Linedef (V2 Float) (Maybe (Sidedef Sector))
            -> [GLfloat]
linedefLine offset scale Linedef{..} =
  let (V2 x1 y1) = (offset + startVertex) * pure scale
      (V2 x2 y2) = (offset + endVertex) * pure scale
      z = 1.0 * scale
  in [x1, y1, z, x2, y2, z]

renderableVs :: V2 Float -> GLfloat -> DoomMap -> [GLfloat]
renderableVs offset scale DoomMap{..} = foldMap (linedefLine offset scale) (toList linedefs)


linedef3D :: Linedef (V2 Float) (Maybe (Sidedef Sector))
          -> [GLfloat]
linedef3D Linedef{..} =
  let (V2 x1 y1) = startVertex
      (V2 x2 y2) = endVertex
      sideR = case rightSidedef of
        Nothing -> []
        Just Sidedef{..} -> let z1 = fromIntegral (floorHeight sector)
                                z2 = fromIntegral (ceilingHeight sector)
                            in [ x1, y1, z1, x2, y2, z1
                               , x1, y1, z2, x2, y2, z2 ]
      sideL = case leftSidedef of
        Nothing -> []
        Just Sidedef{..} -> let z1 = fromIntegral (floorHeight sector)
                                z2 = fromIntegral (ceilingHeight sector)
                            in [ x1, y1, z1, x2, y2, z1
                               , x1, y1, z2, x2, y2, z2 ]
  in sideR <> sideL


sectorsPlane :: (Vector (V3 Float), Sector)
             -> [GLfloat]
sectorsPlane (vs, _) = foldMap (\(V3 x y z) -> [x, y, z]) vs


data View = View
  { _pos :: V3 GLfloat
  , _posD :: V3 GLfloat
  , _camF :: V3 GLfloat
  , _camU :: V3 GLfloat
  , _hAngD :: GLfloat
  , _vAngD :: GLfloat
  } deriving (Eq, Ord, Show)


-- runApp :: (Storable a, Integral a) => SVector (V2 a) -> IO ()
runApp :: DoomMap -> IO ()
runApp dm = do
  inited <- GLFW.init
  when (not inited) (die "Error in glfwInit")

  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core


  camera <- newIORef $ ( Camera (V3 0 0 0) (V3 0 0 1) (V3 0 1 0)
                       , CameraDelta (V3 0 0 0) 0 0)
  mouse <- newIORef (0,0)

  window <- GLFW.createWindow 1024 768 "ShowDoom" Nothing Nothing

  case window of
    Nothing -> do
      putStrLn "Failed to open GLFW window"
      GLFW.terminate
      exitFailure

    Just win -> do
      GLFW.makeContextCurrent window
      GLFW.setStickyKeysInputMode win GLFW.StickyKeysInputMode'Enabled

      programId <- loadShaders "shaders/map_vertexes.vert" "shaders/map_vertexes.frag"

      let vs' = renderableVs (V2 (-2400) (2000)) (1/4000) dm

      _ <- generateVertexArray
      vBuffer <- bindBuffer vs'

      glClearColor 0 0 0.4 0
      forever $ do
        glClear GL_COLOR_BUFFER_BIT
        glUseProgram programId

        let view = createMatrix

        glEnableVertexAttribArray 0
        glBindBuffer GL_ARRAY_BUFFER vBuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

        glDrawArrays GL_LINES 0 (fromIntegral $ length vs')
        glDisableVertexAttribArray 0

        GLFW.swapBuffers win
        GLFW.pollEvents

        escDown <- (GLFW.KeyState'Pressed ==) <$> GLFW.getKey win GLFW.Key'Escape
        shdClose <- GLFW.windowShouldClose win

        when (escDown || shdClose) $ GLFW.terminate >> exitSuccess
