module Main (main) where

import qualified Blog as B
import qualified PostManagement as PM

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if shouldRunUpdate args then PM.main
    else B.main


shouldRunUpdate :: [String] -> Bool
shouldRunUpdate = elem "run_update"