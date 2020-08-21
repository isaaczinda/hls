module Main where

import Parser
import AST
import TypecheckStatement
import TypecheckBase
import System.Environment
import Control.Monad


-- runs a program and performs all printing
run :: String -> IO ()
run filename = do
    code <- readFile filename

    let ast = parse block code
    let (_, errs) = typecheckBlock ast (emptyFrame, code)
    mapM putStrLn errs
    return ()

verifyArgs :: [String] -> String
verifyArgs args =
    case (length args) of
        0 -> error "must pass name of program to be run."
        1 -> (head args)
        otherwise -> error "too many arguments."

main :: IO ()
main = do
    args <- getArgs
    filename <- return (verifyArgs args)
    run filename
