module Evaluator where
import Lexer
import Parser
import qualified Data.Map as M

-- Symbol Table Stuffs and helper functions
type SymTable = M.Map String Double

addVar :: String -> Double -> SymTable -> ((), SymTable)
addVar str val symTab = let symTab' = M.insert str val symTab
                         in ((),symTab')

lookUp :: String -> SymTable -> (Double, SymTable)
lookUp str symTab = case M.lookup str symTab of 
    Just val -> (val,symTab)
    Nothing -> error $ "Undefined variable" ++ str

-------------------

-------------------

-- helper function for evaluating trig functions
getTrigVal :: String -> String -> Double
getTrigVal func rads  = case func of
    "Sin" -> sin (read rads)
    "Cos" -> cos (read rads)
    "Tan" -> tan (read rads)

    --Evaluator part
    -- Almost all function implementations are trivial
evaluate :: Tree -> SymTable ->(Double,SymTable)
evaluate (SumNode op left right) symTab = 
    let (leftExpr,symTab') = evaluate left symTab
        (rightExpr,symTab'') = evaluate right symTab'
     in case op of
         Plus -> (leftExpr + rightExpr,symTab'')
         Minus -> (leftExpr - rightExpr , symTab'')

evaluate (ProdNode op left right) symTab=
    let (leftExpr,symTab') = evaluate left symTab
        (rightExpr,symTab'') = evaluate right symTab'
     in case op of
         Times -> (leftExpr * rightExpr , symTab'') 
         Div -> (leftExpr / rightExpr , symTab'')
         Mod -> (leftExpr - (fromIntegral $ truncate $ (leftExpr / rightExpr)) * rightExpr , symTab'')

evaluate (UnaryNode op tree) symTab =
    let (x,symTab') = evaluate tree symTab
     in case op of
         Plus -> (x,symTab')
         Minus -> (-x,symTab')

evaluate (TrigNode trigFunc exprTree) symTab =
    let (x , symTab') = evaluate exprTree symTab
     in ( getTrigVal (show trigFunc) (show x) ,symTab')


evaluate (NumNode number) symTab = (number,symTab)

evaluate (AssignNode str tree) symTab =  
    let (val , symTab') = evaluate tree symTab
        (_ , symTab'') = addVar str val symTab
     in (val,symTab'')


evaluate (VarNode str) symTab = lookUp str symTab

