{-# LANGUAGE PatternGuards #-}
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
-----------------------------------------------------------------------------
module Main (
    -- * main function.
    main
  , mainuistr
) where

import Data.List (nub)

import System.IO (hPutStrLn, stderr)
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.Console.GetOpt

import qualified Graphics.UI.MainUI as UI

import Paths_hsgame

data Flag = Help
          | N Int
  deriving Eq

-- | Temporary place to put this string.
mainuistr :: String
mainuistr = "ui/gameui.glade"

-- | main
main :: IO ()
main = do (_, _) <- parseArgs -- return ([String], Int), (files, l)
          -- Open and show main ui
          UI.loadUI mainuistr

-- | Usage String
usage :: String
usage = "Usage: game [-h] [-n n] [file ...]"

-- | Command line options definition.
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Show this help message",
            Option ['n'] []       (ReqArg (\s -> N (read s)) "N")
                   "Number (default 1)" ]

-- | Parse args from stdin and handle accordingly.
--
parseArgs :: IO ([String], Int)
parseArgs = do
      argv <- getArgs
      case parse argv of
          ([], files, [])                     -> return (nub files, 0)
          (opts, files, [])
              | Help `elem` opts              -> help
              | [N n] <- filter (/=Help) opts -> return (nub files, n)
          (_,_,errs)                          -> die errs
    where
      parse argv = getOpt Permute options argv
      header     = usage
      info       = usageInfo header options
      dump       = hPutStrLn stderr
      die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
      help       = dump info                  >> exitWith ExitSuccess

