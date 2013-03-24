{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- module      :  main
-- copyright   :
-- license     :
--
-- maintainer  :  creichert07@gmail.com
-- stability   :  unstable
-- portability :  portable
--
-----------------------------------------------------------------------------
module Main (main) where

import Data.List (nub)

import System.IO (hPutStrLn, stderr)
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.Console.GetOpt

import qualified Graphics.UI.MainUI as UI


data Flag = Help
          | N Int
  deriving Eq


mainui :: String
mainui = "ui/gui.glade"


main :: IO ()
main = do (files, k) <- parseArgs -- files is a list of files, k is from -n

          -- Open the main ui
          UI.loadUI mainui
          putStrLn "Here"


usage :: String
usage = "Usage: gui [-h] [-n n] [file ...]"


options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Help!",
            Option ['n'] []       (ReqArg (\s -> N (read s)) "N")
                   "Number (default 1)" ]


-- | This function was copied from the "Real World Haskell" getOpt
-- example.
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

