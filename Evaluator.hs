module Evaluator where
import Lexer
import Parser
import qualified Data.Map as M
import Data.Maybe
import Prelude hiding (mod)
import LogicalOperators
import MaybeArithmeticOperators


-- Symbol Table Stuffs and helper functions
type SymTable a= M.Map String a

addVar :: String -> Maybe a -> SymTable a  -> ((), SymTable a)
addVar str val symTab = 
    case val of
        Just x -> ((), M.insert str x symTab) 
        Nothing -> ((),symTab)



--addVar str val symTab = let symTab' = M.insert str val symTab
 --                        in ((),symTab')

lookUp :: String -> SymTable a -> (a, SymTable a)
lookUp str symTab = case M.lookup str symTab of 
    Just val -> (val,symTab)
    Nothing -> error $ "Undefined variable" ++ str

-------------------

-------------------

-- helper function for evaluating trig functions
getTrigVal :: String -> Double -> Maybe Double
getTrigVal func rads  = case func of
    "Sin" -> Just ( sin rads) 
    "Cos" -> Just  (cos rads)
    "Tan" -> Just (tan rads)

--- top level evaluate function

evaluate :: Tree -> SymTable Bool -> SymTable Double -> (Maybe Bool ,Maybe Double, SymTable Bool ,SymTable Double) 

evaluate parseTree symB symD =
    let applyFunc = (\x -> x parseTree symB symD)
     in case parseTree of
        SumNode op expLeft expRight -> applyFunc arithEval
        ProdNode op expLeft expRight -> applyFunc arithEval
        CmpNode op expLeft expRight -> applyFunc logicEval
        LogicalNodeBinary op expLeft expRight ->applyFunc logicEval 
        LogicalNodeUnary op exp -> applyFunc logicEval 
        LnNode exp -> applyFunc arithEval
        LogNode left right -> applyFunc arithEval
        TrigNode func expr -> applyFunc arithEval
        NumNode x -> applyFunc arithEval
        BoolNode x -> applyFunc logicEval
        VarNode x -> applyFunc arithEval
        AssignNode str valD -> applyFunc arithEval


--helper function for compare


compareAndReturn op a b = case op of
    Equal -> Just (a == b)
    NotEqual -> Just (a /= b)
    LessThan -> Just (a < b)
    LessOrEq -> Just (a <= b)
    GrtrThan -> Just ( a > b)
    GrtrOrEq -> Just (a >= b)




-- Following are the evaluators for Logical Operations
logicEval :: Tree -> SymTable Bool -> SymTable Double -> (Maybe Bool,Maybe Double,SymTable Bool , SymTable Double)
logicEval (CmpNode op left right) symB symD=
    let (leftExprB,leftExprD,symB',symD') =  evaluate left symB symD
        (rightExprB,rightExprD,symB'',symD'') = evaluate right symB' symD'
        returnFunc a b = (compareAndReturn op a b , Nothing, symB'',symD'')
     in case (leftExprB , leftExprD , rightExprB, rightExprD) of
         (Just x , _ , Just y , _) -> returnFunc x y
         (_ , Just x , _ , Just y) -> returnFunc x y
         (_ , _ , _ , _)           -> (Nothing , Nothing , symB'' , symD'')



logicEval (BoolNode a) symB symD = (Just a ,Nothing, symB,symD)

logicEval (LogicalNodeBinary op left right) symB symD =
    let (leftExprB,leftExprD,symB',symD') = evaluate left symB symD
        (rightExprB,rightExprD,symB'',symD'') = evaluate right symB' symD'
        func x = (leftExprB >>= (\y -> rightExprB >>= (\z -> x y z)))
        val= case op of 
             And -> func myAnd 
             Or ->  func myOr
             Xor -> func xor
             Implies -> func implies 
             DoubleImplies ->  func doubleImplies
     in (val , Nothing , symB'' , symD'')


logicEval (LogicalNodeUnary op expr) symB symD = 
    let (exprB , exprD , symB',symD') = evaluate expr symB symD
     in case exprB of
         Just val -> (Just (not val), Nothing, symB' , symD')
         Nothing -> (Nothing,Nothing , symB',symD')


-- THis handles all the arithmetic operations
    --Evaluator part
    -- Almost all function implementations are trivial
    --
arithEval :: Tree -> SymTable Bool ->SymTable Double -> (Maybe Bool , Maybe Double, SymTable Bool , SymTable Double)

arithEval (SumNode op left right) symB symD =
    let (leftB , leftD , symB' , symD') = evaluate left symB symD
        (rightB, rightD , symB'',symD'') = evaluate right symB' symD'
        x = case op of
             Plus  ->  (leftD >>= (\y -> rightD >>= (\z -> add y z)))
             Minus ->  (leftD >>= (\y -> rightD >>= (\z -> add y z)))
     in (Nothing , x ,symB'' , symD'')






arithEval (ProdNode op left right) symB symD =
    let (leftB,leftD,symB',symD') = evaluate left symB symD
        (rightB,rightD,symB'',symD'') = evaluate right symB' symD'
        val = case op of
            Times -> (leftD >>= (\y -> rightD >>= (\z -> (multiply y z))))
            Div   -> (leftD >>= (\y -> rightD >>= (\z -> (divide y z))))
            Mod   -> (leftD >>= (\y -> rightD >>= (\z -> (mod y z))))
            Pow   -> (leftD >>= (\y -> rightD >>= (\z -> (power y z))))
     in (Nothing , val , symB'', symD'') 


arithEval (UnaryNode op expr) symB symD=
    let (exprB, exprD , symB' , symD') = evaluate expr symB symD
        func x = (exprD >>= (\z -> x z))
        val = case op of
            Plus -> func (multiply 1)
            Minus -> func (multiply (-1)) 
     in (Nothing , val , symB' , symD')
         




arithEval (TrigNode trigFunc exprTree) symB symD =
    let (xB ,xD,  symB' , symD') = evaluate exprTree symB symD
        func x = (xD >>= (\z -> x z))
        val = func (getTrigVal (show trigFunc))
     in (Nothing, val , symB' , symD')


arithEval (LogNode logbase operand) symB symD = 
    let (_, logbase' ,symB',symD') = evaluate logbase symB symD
        (_,operand' , symB'', symD'') = evaluate operand symB' symD'
        func x = (logbase' >>= (\y -> operand' >>=  (\z -> x y z)))
        val = func myLogBase
     in (Nothing , val , symB'' , symD'')

arithEval (LnNode operand) symB symD =
    let (_ , operand', symB' , symD') =  evaluate operand symB symD
        val = (operand' >>= (\x -> myLogBase (exp 1)  x))
     in (Nothing, val, symB' , symD')



arithEval (NumNode number) symB symD = (Nothing , Just number , symB, symD)

arithEval (AssignNode str tree) symB symD =  
    let (xB,xD,symB',symD') = evaluate tree symB symD
        (_ , symD'') = addVar str xD symD
     in (Nothing , xD, symB' , symD'')

arithEval (VarNode str) symB symD = 
    let (x, symD') = lookUp str symD
     in (Nothing , Just x , symB, symD')



