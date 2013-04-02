-----------------------------------------------------------------------------
-- |
-- module      :  Graphics.UI.MainUI
-- copyright   :  (c) Christpher Reichert <creichert07@gmail.com>
-- license     :  BSD3
--
-- maintainer  :  creichert07@gmail.com
-- stability   :  unstable
-- portability :  portable
--
-- The Main User Interface for <this Appliation>.
--
-----------------------------------------------------------------------------
module Graphics.UI.MainUI(
    -- * loadUI Builds and displays this ui. Also starts Gtk Event Loop.
    loadUI
) where

import Control.Monad.Trans ( liftIO )

import qualified Graphics.UI.Gtk as Gtk

import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.OpenGL ( GLDrawingArea )

import Graphics.Engine

data MainUI = MainUI { mainWindow  :: Gtk.Window
                     , glCanvas      :: GLDrawingArea }


-- | Loads the main ui file for hsbeets.
--loadUI :: FilePath -> IO ()
loadUI gladepath = do
        Gtk.initGUI

        (canvas, state) <- initEngine

        builder <- builderNew
        builderAddFromFile builder gladepath

        ui <- buildUI builder canvas state

        Gtk.widgetShowAll $ mainWindow ui

        Gtk.mainGUI

-- | Buils the UI Given a Builder and a GLDrawingArea
-- from the Engine.
--buildUI :: Builder -> GLDrawingArea -> ProgramState -> IO MainUI
buildUI builder canvas state = do

        win <- builderGetObject builder Gtk.castToWindow "mainWindow"
        Gtk.onDestroy win Gtk.mainQuit

        -- Get the Configuration layout.
        box <- builderGetObject builder Gtk.castToVBox "canvasbox"
        Gtk.boxPackStart box canvas Gtk.PackGrow 0

        win `Gtk.on` Gtk.keyPressEvent $ Gtk.tryEvent $ do
                        mod <- Gtk.eventModifier -- We could take a list of modifiers (mod:mods)
                        key <- Gtk.eventKeyName
                        val <- Gtk.eventKeyVal
                        liftIO $ keyEvent state True key mod val

        win `Gtk.on` Gtk.keyReleaseEvent $ Gtk.tryEvent $ do
                        mod <- Gtk.eventModifier -- We could take a list of modifiers (mod:mods)
                        key <- Gtk.eventKeyName
                        val <- Gtk.eventKeyVal
                        liftIO $ keyEvent state False key mod val

        return $ MainUI win canvas

