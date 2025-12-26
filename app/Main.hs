module Main (main) where

import Lib
import System.Environment( getArgs)
import Frontend.Parser
import Eval

main :: IO ()
main = do
    fullArgs <- getArgs
    parseRes <- parseFile $ fullArgs !! 0
    let val = eval parseRes
    let res = show val
    in putStrLn res

