module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand
import Data.Maybe 
import Sugar
import Eval


main :: IO ()
main = do 
    putStr "miniHaskell> "
    hFlush stdout
    input <- getLine
    let cmd = fromJust (parseFirst replCommand input)
    if isQuit cmd then return ()
    else 
        if isLoad cmd then main
        else do
            let str = fromJust (fromEval cmd)
            if str == "" then main
            else do
                let output = parseFirst exprParser str 
                if isNothing output then
                    putStrLn "Invalid Expression"
                else
                    putStrLn (showExp (sugarExp (normalize (desugarExp (fromJust output)))))
                main

