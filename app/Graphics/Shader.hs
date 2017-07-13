module Graphics.Shader
       ( loadShaders
       , getShaderAttrib
       , getShaderUniform
       ) where

import ClassyPrelude
import Control.Monad (when)
import qualified Data.ByteString.Char8 as C

import Foreign

import Graphics.GL

compileShader :: FilePath -> GLuint -> IO ()
compileShader fp shader = do
  putStrLn $ "compiling shader: " <> tshow fp
  code <- C.readFile fp
  C.useAsCString code
    (\ptr -> do
        -- this is because glShaderSource can take _many_ strings of source.
        withArray [ptr] $ \srcs -> glShaderSource shader 1 srcs nullPtr)
  glCompileShader(shader)
  -- check vertex shader
  alloca $ \ptr -> do
    glGetShaderiv shader GL_INFO_LOG_LENGTH ptr
    len <- peek ptr
    when (len > 0) $ alloca $ \msg -> do
      glGetShaderInfoLog shader len nullPtr msg
      msg' <- C.unpack <$> C.packCString msg
      putStrLn $ tshow msg'

loadShaders :: FilePath -> FilePath -> IO GLuint
loadShaders vfp ffp = do
  vertexShaderId <- glCreateShader GL_VERTEX_SHADER
  fragmentShaderId <- glCreateShader GL_FRAGMENT_SHADER

  compileShader vfp vertexShaderId
  compileShader ffp fragmentShaderId

  putStrLn "linking program"
  programId <- glCreateProgram
  glAttachShader programId vertexShaderId
  glAttachShader programId fragmentShaderId
  glLinkProgram programId

  -- check program
  alloca $ \ptr -> do
    glGetProgramiv programId GL_INFO_LOG_LENGTH ptr
    len <- peek ptr
    when (len > 0) $ alloca $ \msg -> do
      glGetProgramInfoLog fragmentShaderId len nullPtr msg
      msg' <- C.unpack <$> C.packCString msg
      putStrLn $ tshow msg'

  glDetachShader programId vertexShaderId
  glDetachShader programId fragmentShaderId
  glDeleteShader vertexShaderId
  glDeleteShader fragmentShaderId

  pure programId

getShaderAttrib :: GLuint -> ByteString -> IO GLint
getShaderAttrib prog attribName = do
  location <- C.useAsCString attribName $ \attribNameCString ->
    glGetAttribLocation prog attribNameCString
  when (location == -1) $ error $ "Couldn't bind attrib: " ++ C.unpack attribName
  pure location

getShaderUniform :: GLuint -> ByteString -> IO GLint
getShaderUniform prog uniformName = do
  location <- C.useAsCString uniformName $ \uniformNameCString ->
    glGetUniformLocation prog uniformNameCString
  when (location == -1) $ error $ "Couldn't bind uniform: " ++ C.unpack uniformName
  pure location
