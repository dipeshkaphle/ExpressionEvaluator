module Lexer where
import Data.Char

-- & | ~ --> <--> ^^ will be the representations respectively
data LogicalOps = And | Or | Not | Implies | DoubleImplies | Xor deriving (Show, Read , Eq)

-- == != > >= < <=
data CompOps = Equal | NotEqual | GrtrThan | GrtrOrEq | LessThan | LessOrEq  deriving (Show , Eq , Read)



data Operator = Plus | Minus | Times | Div | Mod | Pow
    deriving (Show, Eq)

data Trig =  Sin  | Cos  | Tan deriving (Show, Eq, Read)

data Logarithm = Log | Ln deriving (Show , Eq, Read)

data Token = TokOp Operator
           | TokCmp CompOps
           | TokLogicalBinary LogicalOps
           | TokLogicalUnary LogicalOps
           | TokAssign
           | TokLParen
           | TokRParen
           | TokBool Bool
           | TokTrig Trig 
           | TokLog Logarithm
           | TokIdent String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)


compoperator:: String -> CompOps
compoperator str
  | str == "==" = Equal
  | str == "!=" = NotEqual
  | str == ">" = GrtrThan
  | str == ">=" = GrtrOrEq
  | str == "<" = LessThan
  | str == "<=" = LessOrEq

logicaloperator :: String -> LogicalOps
logicaloperator str
  | str== "&" = And
  | str == "|" = Or
  | str == "~" = Not
  | str == "^^" = Xor
  | str == "-->" = Implies
  | str == "<-->" = DoubleImplies


operator :: String -> Operator
operator c | c == "+" = Plus
           | c == "-" = Minus
           | c == "*" = Times
           | c == "/" = Div
           | c == "%" = Mod
           | c == "^" = Pow
 

isMySymbol c = elem c "=<>&|~-^+*%/!"

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | isMySymbol c = tokenizeOperator c cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = keyword c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]



tokenizeOperator :: Char -> String -> [Token]
tokenizeOperator c cs = let (theOperator , remainingStuff) = span (\x -> isMySymbol x) (c:cs)
                            (_, remainingStuff') = span isSpace remainingStuff
                         in case theOperator of
                             str | elem str ["+","-","*","/","%","^"] -> TokOp (operator str) : tokenize remainingStuff'
                             str | elem str ["&","|","^^","-->","<-->"] -> TokLogicalBinary (logicaloperator str) :tokenize remainingStuff'
                             str | str == "~" -> TokLogicalUnary Not :tokenize remainingStuff'
                             str | elem str ["==","!=",">",">=","<","<="] -> TokCmp (compoperator str) : tokenize remainingStuff'
                             "=" -> TokAssign : tokenize remainingStuff'
                             _ -> error $ "Invalid Operator: " ++ theOperator



keyword :: Char -> String -> [Token]
keyword c cs = let (rem,cs') = span isAlphaNum cs
                   (_,cs'') = span isSpace cs'
                in case (c:rem) of
                    theWord | elem theWord ["Sin","Cos","Tan"] -> TokTrig (read theWord) : tokenize cs''
                    theWord | elem theWord ["Log", "Ln"] -> TokLog (read theWord) : tokenize cs''
                    theWord | elem theWord ["True","False"] -> TokBool (read theWord) : tokenize cs''
                    _ -> TokIdent (c:rem) : tokenize cs''


    
-- almost redundant but im not sure yet so you  stay for now
identifier c cs = let (name, cs') = span isAlphaNum cs 
                   in TokIdent (c:name) : tokenize cs'

number :: Char -> String -> [Token]
number c cs = 
   let (digs, cs') = span (\x -> (isDigit x || x=='.')) cs in
   TokNum (read (c : digs)) : tokenize cs'

