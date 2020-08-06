module Main where
import LexAndParse
import Evaluator
import qualified Data.Map as M

main = do
    loop (M.fromList [("pi",pi),("e",exp 1.0)])

loop symTab = do
    str <- getLine
    if null str
       then return ()
       else
        let toks = tokenize str
            expressionTree = parse toks
            (val, symTab') = evaluate expressionTree symTab
         in do
             print val
             loop symTab'




