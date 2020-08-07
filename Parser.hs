module Parser where
import Lexer
---- parser ----

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | LnNode Tree
          | LogNode Tree Tree
          | TrigNode Trig Tree
          | NumNode Double
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
   Expression <- Term [-+] Expression
                | Term (identifier in this case ) = Expression
                | Term

    Hence we parse in this particular order
-}
expression :: [Token] -> (Tree, [Token])
expression toks = 
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Plus, Minus] -> 
             let (term' , toks'') = term (accept toks')
              in case lookAhead toks'' of
                  TokOp op' | elem op' [Plus, Minus] -> let (tree'' , toks''') = expression (accept toks'')
                                                         in ((SumNode op' (SumNode op termTree term') tree''), toks''')
                  _ -> (SumNode op termTree term', toks'')
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
      _ -> error $ "Parse error on token: " ++ show toks


-- parses the parse tree from given tokens
parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'
