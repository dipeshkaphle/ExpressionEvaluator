module Parser where
import Lexer
---- parser ----

-- This is so not Haskell like
-- The functions should really not be this long

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | CmpNode CompOps Tree Tree
          | LogicalNodeBinary LogicalOps Tree Tree
          | LogicalNodeUnary LogicalOps Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | LnNode Tree
          | LogNode Tree Tree
          | TrigNode Trig Tree
          | NumNode Double
          | BoolNode Bool
          | VarNode String
    deriving Show

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts



{-
   Expression has a structure like this
   Expression <- Term [-+] Term (some op) expr
                | Term [Logical or Cmp] Expr
                | Term (identifier in this case ) = Expression
                | Term

    Hence we parse in this particular order
-}

 -- This looks complicated because it fixes associativity problem for +-
 ---- instead of normal term +- expr grammar we use term+-term+-expr  something else 

  -- if something else doesnt exist  then we follow normal term +- expr grammar
  --
 -- this seems to generate correct  prefix expression
 --
 --
 -- This is complicated. Im trying to make evaluation order python repl like
 -- Like without nesting in the TokOp case we wont be able to parse things like 1+2<3 or 1+2 & 3
 -- Always using parens is encouraged
 -- The associativity and the order of evaluation can get kinda weird
 -- I have tried my best to do this correctly with how much I know
 -- Ill try to make this better 
expression :: [Token] -> (Tree, [Token])
expression toks = 
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
         (TokLogicalBinary op) -> let (exprTree, toks'') =  expression (accept toks')
                                    in (LogicalNodeBinary op termTree exprTree , toks'')
         (TokOp op) | elem op [Plus, Minus] -> 
             let (term' , toks'') = term (accept toks') 
              in case lookAhead toks'' of              
                  TokOp op' | elem op' [Plus, Minus] -> let (tree'' , toks''') = expression (accept toks'')                                     
                                                         in ((SumNode op' (SumNode op termTree term') tree''), toks''') 
                  TokCmp op' -> let (tree'',toks''') = expression (accept toks'')
                                 in ((CmpNode op' (SumNode op termTree term') tree'') , toks''')
                  TokLogicalBinary op' -> let (tree'',toks''') = expression (accept toks'')
                                          in ((LogicalNodeBinary op' (SumNode op termTree term') tree''), toks''')
                  _ -> (SumNode op termTree term', toks'')

         (TokCmp op) -> let (exprTree , toks'') = expression (accept toks')
                         in (CmpNode op termTree exprTree, toks'')
         TokAssign ->
            case termTree of
               VarNode str -> 
                  let (exTree, toks'') = expression (accept toks') 
                  in (AssignNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         _ -> (termTree, toks')

{-
   Term has the following grammar
    Term <- factor [*/%] Term 
            | factor
    
-}

term :: [Token] -> (Tree, [Token])
term toks = 
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div, Mod , Pow] ->
            let (termTree, toks'') = term (accept toks') 
            in (ProdNode op facTree termTree, toks'')
         _ -> (facTree, toks')

    {-
       Factor's grammar is vast. Contains a lot of rules
        Factor <- TokNum Double (i.e a standalone number)
                 | TokIdent str (i.e an identifier as in variable name)
                 | unaryOperation Factor (i.e something like -x , -2 , -(x+y) . The unary operation can only be + or - ofc)
                 | '(' expression ')' (i.e an expression enclosed inside parentheses)
                 |  TrigNode Trig Tree (something like TrigNode Sin (NumNode 10))
                 | log term term
                 | ln term
    -}


factor :: [Token] -> (Tree, [Token])
factor toks = 
   case lookAhead toks of
      (TokNum x)     -> (NumNode x, accept toks)
      (TokBool x)    -> (BoolNode x , accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Plus, Minus] -> 
            let (facTree, toks') = factor (accept toks) 
            in (UnaryNode op facTree, toks')
      (TokLog logType) -> case logType of
          Log -> let (logbase,toks') =term (accept toks)
                     (logOperand , toks'') = term toks'
                  in (LogNode logbase logOperand, toks'')
          Ln -> let (logOperand , toks') = term (accept toks)
                 in (LnNode logOperand, toks')
      (TokTrig func) -> let (exprTree ,toks') = term (accept toks) 
                                    in (TrigNode func exprTree, toks')
      TokLParen      -> 
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      TokLogicalUnary op -> let (exprTree, toks')= expression (accept toks)
                             in (LogicalNodeUnary op exprTree, toks')  -- op is always going to be Not in this case
      _ -> error $ "Parse error on token: " ++ show toks


-- parses the parse tree from given tokens
parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'
