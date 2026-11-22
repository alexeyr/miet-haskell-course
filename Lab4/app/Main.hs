module Main where

import CLI (parserInfo, run)
import Options.Applicative (execParser)

main :: IO ()
main = execParser parserInfo >>= run
