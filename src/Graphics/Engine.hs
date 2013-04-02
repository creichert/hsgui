-----------------------------------------------------------------------------
-- |
-- module      :  Graphics.Engine
-- copyright   :  (c) Christpher Reichert <creichert07@gmail.com>
-- license     :  BSD3
--
-- maintainer  :  creichert07@gmail.com
-- stability   :  unstable
-- portability :  portable
--
-- This is the core Engine for the App.
--
-----------------------------------------------------------------------------
module Graphics.Engine (
    ProgramState(..)

    -- * Initialize a new OpenGL engine.
  , initEngine

    -- * Handle key event
  , keyEvent
) where

import Data.IORef
import qualified Data.IntSet as IS

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import Graphics.Rendering.OpenGL as GL


data ProgramState = PS { keysPressed :: IS.IntSet
                       , px          :: GL.GLfloat
                       , py          :: GL.GLfloat
                       , pz          :: GL.GLfloat
                       , dx          :: GL.GLfloat
                       , dy          :: GL.GLfloat
                       , dz          :: GL.GLfloat }
    deriving Show

deltaV = 0.02
deltaH = 0.02

-- | Initialize our OpenGL drawing enging and
-- return a GLDrawingArea
--initEngine :: IO GtkGL.GLDrawingArea -- ^ Returns a GLDrawingArea for this Engine.
--initEngine state = do
initEngine = do
        _ <- GtkGL.initGL

        glconf <- GtkGL.glConfigNew [ GtkGL.GLModeRGBA
                                    , GtkGL.GLModeDepth
                                    , GtkGL.GLModeDouble ]

        canvas <- GtkGL.glDrawingAreaNew glconf

        -- Initialise some GL setting just before the canvas first gets shown
        -- (We can't initialise these things earlier since the GL resources that
        -- we are using wouldn't heve been setup yet)
        Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
                    clearColor $= (Color4 0.0 0.0 0.0 0.0)
                    matrixMode $= Projection
                    loadIdentity
                    ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
                    depthFunc $= Just Less
                    drawBuffer $= BackBuffers


        -- Temp spot to create state. This will most likely be held elsewhere in the app later.
        state <- newIORef $ PS { keysPressed = IS.empty
                               , px          = 0
                               , py          = 0
                               , pz          = 5.0
                               , dx          = 0
                               , dz          = 0 }

        -- Set the repaint handler
        Gtk.onExpose canvas $ \_ -> do
                    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
                      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
                      display state -- draw the opengl function
                      GtkGL.glDrawableSwapBuffers glwindow
                    return True

        -- Setup the animation
        Gtk.timeoutAddFull (do
                update state
                --Gtk.widgetQueueDraw canvas
                return True)
          Gtk.priorityDefaultIdle 3 -- last var is animation wait time

        return (canvas, state)


-- | Called when OpenGL timeout signal is called.  update :: IORef ProgramState -> IO ()
update state = do
    ps@PS { } <- readIORef state
    return ()


-- | Dummy function to test OpenGL by rendering a simply polygoon
display state = do

    loadIdentity
    color (Color3 1 1 1 :: Color3 GLfloat)
    -- Instead of glBegin ... glEnd there is renderPrimitive.
    renderPrimitive Polygon $ do
      vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
      vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
      vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
      vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)


--keyEvent state rel mods val name char = do
keyEvent state rel name mod val = do
    ps@PS { keysPressed = kp
          , dx          = dx
          , dy          = dy
          , dz          = dz
          , px          = px
          , py          = py
          , pz          = pz } <- readIORef state

    -- Only process the key event if it is not a repeat
    --if (fromIntegral val `IS.member` kp && rel) ||
    --   (fromIntegral val `IS.notMember` kp && not rel)
    --   then do
    let return' ps b = do
          -- maintain list of currently pressed keys
          writeIORef state $!
            if rel
              then ps { keysPressed = fromIntegral val `IS.insert` kp }
              else ps { keysPressed = fromIntegral val `IS.delete` kp }
          return b
        -- accept/decline to handle the key event
        accept ps  = return' ps True
        decline ps = return' ps False

    putStrLn $ unwords [name , show rel]  -- debugging
    -- process keys
    case rel of
      -- on PRESS only
      True
        | name == "w"      -> accept $ ps { dy = dy + deltaV }
        | name == "a"      -> accept $ ps { dz = dz - deltaV }
        | name == "s"      -> accept $ ps { dx = dx + deltaV }
        | name == "d"      -> accept $ ps { dx = dx - deltaV }
        | otherwise        -> decline ps
      -- on RELEASE only
      False
        | name == "Escape" -> Gtk.mainQuit >> accept ps
        | name == "w"      -> accept $ ps { dx = dx - deltaV }
        | name == "a"      -> accept $ ps { dz = dz - deltaV }
        | name == "s"      -> accept $ ps { dx = dx + deltaV }
        | name == "d"      -> accept $ ps { dz = dz + deltaV }
        | otherwise        -> decline ps

    return ()

