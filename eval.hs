import Lexer
import Parser
import Evaluator
import qualified Data.Map as M
import System.Environment
import Prelude hiding (mod)
import MaybeArithmeticOperators
import LogicalOperators
import System.IO

main = do
    str <- getArgs
    if null str
       then loop (M.empty) (M.fromList [("pi", pi), ("e",exp 1.0)])
       else loop' (M.empty) (M.fromList [("pi",pi),("e",exp 1.0)])



getVal :: Maybe Bool -> Maybe Double -> IO ()
getVal (Just x) _ = print (x)
getVal _ (Just x) = print (x)
getVal _ _ = putStrLn "Cant evaluate "



processAndPrint str symB symD =
        let toks = tokenize str
            expressionTree = parse toks
            (valB, valD , symB' , symD') = evaluate expressionTree symB symD
         in do
             getVal valB valD


loop' symB symD = do
    str <- getArgs
    processAndPrint (str !! 0) symB symD
    



prompt text = do
    putStr text
    hFlush stdout
    getLine


loop symB symD = do
    str <- prompt ">>>> "
    if null str
       then return ()
       else
        let toks = tokenize str
            expressionTree = parse toks
            (valB, valD , symB' , symD') = evaluate expressionTree symB symD
         in do
             getVal valB valD
             loop symB' symD'




