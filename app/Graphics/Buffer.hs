module Graphics.Buffer where

import ClassyPrelude
import Graphics.GL


import Foreign

bindBuffer :: Storable a => [a] -> IO GLuint
bindBuffer vs = alloca $ \ptr -> do
  glGenBuffers 1 ptr
  glBindBuffer GL_ARRAY_BUFFER =<< peek ptr
  withArray vs $ \arr -> do
    glBufferData
      GL_ARRAY_BUFFER
      (fromIntegral (sizeOf (unsafeHead vs) * length vs))
      (castPtr arr)
      GL_STATIC_DRAW
  peek ptr


generateVertexArray :: IO GLuint
generateVertexArray = alloca $ \ptr -> do
  glGenVertexArrays 1 ptr
  glBindVertexArray =<< peek ptr
  peek ptr


-- A
-- vertexBuffer <- alloca $ \ptr -> do
--         glGenBuffers 1 ptr
--         glBindBuffer GL_ARRAY_BUFFER =<< peek ptr
--         withArray g_vertex_buffer_data $ \arr -> do
--           -- copy vertices to buffer in GPU memory
--           glBufferData
--             GL_ARRAY_BUFFER
--             (fromIntegral (sizeOf (undefined :: GLfloat) * length g_vertex_buffer_data))
--             arr
--             GL_STATIC_DRAW
--         peek ptr
