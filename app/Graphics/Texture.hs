module Graphics.Texture where

import ClassyPrelude
import Foreign hiding (rotate)
import Graphics.GL
import Codec.Picture

loadTexture :: FilePath -> IO GLuint
loadTexture fp = alloca $ \txtPtr -> do
  -- read data, and get pointer to it?
  Right (ImageRGB8 img) <- readTGA fp
  unsafeWith (imageData img) $ \pxsPtr -> do
    glGenTextures 1 txtPtr
    glBindTexture GL_TEXTURE_2D =<< peek txtPtr
    glTexImage2D GL_TEXTURE_2D 0
      (fromIntegral GL_RGB)
      (fromIntegral $ imageWidth img) (fromIntegral $ imageHeight img)
      0 GL_RGB GL_UNSIGNED_BYTE pxsPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  peek txtPtr
