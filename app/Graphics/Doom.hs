module Graphics.Doom where

import ClassyPrelude


import Control.Monad (when, forever)
import System.Exit

import Graphics.GL
import Foreign hiding (rotate)
import Graphics.Shader
import Graphics.Buffer
import Linear.V2 (V2(..))

import qualified Graphics.UI.GLFW as GLFW

transformMapVertexes :: (Storable a, Integral a) => SVector (V2 a) -> [GLfloat]
transformMapVertexes v = foldr
  (\(V2 x y) vs ->
      let x' = (fromIntegral (x - 32768)) / 65536
          y' = (fromIntegral (y - 32768)) / 65536
          z' = 1.0
      in x':y':z':vs) [] (toList v)


runApp :: (Storable a, Integral a) => SVector (V2 a) -> IO ()
runApp vs = do
  inited <- GLFW.init
  when (not inited) (die "Error in glfwInit")

  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

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

      let vs' = transformMapVertexes vs

      traverse_ print (take 30 $ drop 600 vs')
      _ <- generateVertexArray
      vBuffer <- bindBuffer vs'

      glClearColor 0 0 0.4 0
      forever $ do
        glClear GL_COLOR_BUFFER_BIT
        glUseProgram programId

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
