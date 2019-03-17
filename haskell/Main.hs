module Main where

import           LoadBalancer

main :: IO ()
main = run 8000 [Upstream localhost 7878, Upstream localhost 7879]
