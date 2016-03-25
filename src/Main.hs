{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import Control.Monad
import qualified Linear as L
import Data.Array.Storable
import Foreign.Ptr
import qualified Foreign.Marshal.Array as M
import Foreign.C.Types

width, height :: Num a => a
width = 800
height = 600

-- Renders to the given array
render :: Ptr CUInt -> IO ()
render = undefined

-- Uploads the given array to a texture
upload :: Texture -> Int -> Int -> Ptr CUInt -> IO ()
upload texture width height source = do
  (lockedPtr, pitch) <- lockTexture texture Nothing
  let dest = castPtr lockedPtr :: Ptr CUInt
  M.copyArray source dest (width * height * 4)

main :: IO ()
main = do
  -- Initialise all SDL subsystems
  initializeAll

  -- Create SDL window
  window <- createWindow "blue sky" defaultWindow { windowInitialSize = L.V2 width height }
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTexture renderer RGBA8888 TextureAccessStreaming (L.V2 width height)

  -- Create pixel buffer
  pixelBuffer <- newArray (width, height) 0 :: IO (StorableArray Int CUInt)

  -- Render image
  --withStorableArray pixelBuffer render

  -- Main loop
  let loop = do
        -- Poll window events so it doesn't just freeze up
        pollEvents

        -- Draw window
        withStorableArray pixelBuffer (upload texture width height)

        -- Present image
        present renderer

        -- loop until esc is pressed
        keyState <- getKeyboardState
        
        unless (keyState ScancodeEscape) loop

  loop

  -- Clean up
  destroyTexture texture
  destroyRenderer renderer
  destroyWindow window
