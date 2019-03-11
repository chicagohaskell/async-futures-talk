module Main where

import           AsyncTalk

main :: IO ()
main = runBS 8000 fileServer
