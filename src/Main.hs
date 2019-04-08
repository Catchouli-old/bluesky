{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import Control.Monad
import qualified Linear as L
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable (poke)
import qualified Foreign.Marshal.Array as M
import Foreign.C.Types
import Data.Bits

-- | Standard window width and height
width, height :: Num a => a
width = 800
height = 600

-- | Renders to the given array
render :: Ptr CUInt -> IO ()
render buf = forM_ [0..height-1] $ \y -> do 
  forM_ [0..width-1] $ \x -> do 
    let white = CUInt 0xFFFFFFFF
    let idx = width * y + x
    let pix = plusPtr buf (idx*4)
    let col = CUInt . fromIntegral $ x * y
    poke pix col

-- | Uploads the given array to a texture
upload :: SDL.Texture -> Int -> Int -> Ptr CUInt -> IO ()
upload texture width height source = do
  (lockedPtr, pitch) <- SDL.lockTexture texture Nothing
  let dest = castPtr lockedPtr :: Ptr CUInt
  M.copyArray dest source (width * height)
  SDL.unlockTexture texture

-- | Create a window and run the given IO action
withWindow :: (SDL.Window -> SDL.Renderer -> IO ()) -> IO ()
withWindow action = do
  -- Initialise all SDL subsystems
  SDL.initializeAll

  -- Create SDL window
  window <- SDL.createWindow "boop" SDL.defaultWindow { SDL.windowInitialSize = L.V2 width height }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- Do user action
  action window renderer

  -- Clean up
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

-- | Timing info
data TimingInfo = TimingInfo
  { time :: Float -- ^ The current time
  , frames :: Int -- ^ Frames that have passed
  , lastFpsUpdate :: Float -- ^ Last time the FPS was updated
  , fps :: Int -- ^ FPS at that time
  }

-- | Update timing
updateTiming :: TimingInfo -> Float -> TimingInfo
updateTiming timingInfo newTime =
  if newTime - lastFpsUpdate timingInfo >= 1.0
    then timingInfo { time = newTime, lastFpsUpdate = newTime, fps = frames timingInfo, frames = 0 }
    else timingInfo { time = newTime, frames = frames timingInfo + 1 }

-- | Main stuff
main :: IO ()
main = withWindow $ \window renderer -> do
  -- Create texture
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (L.V2 width height)

  -- Create pixel buffer
  pixelBuffer <- newArray (0, width * height - 1) 0 :: IO (StorableArray Int CUInt)

  -- Render image
  withStorableArray pixelBuffer render

  pix <- readArray pixelBuffer (50 * width + 50)
  putStrLn $ "pixel value: " ++ show pix

  -- Main loop
  let loop timingInfo = do
        -- Poll window events so it doesn't just freeze up
        SDL.pollEvents

        -- Draw window
        withStorableArray pixelBuffer (upload texture width height)

        SDL.copy renderer texture Nothing Nothing

        -- Present image
        SDL.present renderer

        -- Update timing info
        newTimingInfo <- updateTiming timingInfo <$> SDL.time

        -- Print fps if changed
        when (lastFpsUpdate timingInfo /= lastFpsUpdate newTimingInfo) $
          putStrLn $ "FPS: " ++ show (fps newTimingInfo)

        -- Loop until esc is pressed
        keyState <- SDL.getKeyboardState
        unless (keyState SDL.ScancodeEscape) (loop newTimingInfo)

  time <- SDL.time
  loop (TimingInfo time 0 time 0)

  SDL.destroyTexture texture
