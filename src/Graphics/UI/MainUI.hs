-----------------------------------------------------------------------------
-- |
-- module      :  Graphics.UI.MainUI
-- copyright   :
-- license     :
--
-- maintainer  :
-- stability   :  unstable
-- portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.MainUI(
    loadUI
) where

import System.Directory

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

data MainUI = MainUI { mainWindow  :: Window }

-- |Loads the main ui file for the gui.
loadUI :: FilePath -> IO ()
loadUI gladepath = do
        initGUI

        builder <- builderNew
        builderAddFromFile builder gladepath

        ui <- buildUI builder

        widgetShowAll $ mainWindow ui

        mainGUI


buildUI :: Builder -> IO MainUI
buildUI builder = do

        mw <- builderGetObject builder castToWindow "mainWindow"
        onDestroy mw mainQuit

        return $ MainUI mw

