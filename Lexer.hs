module Lexer where
import Data.Char

data Operator = Plus | Minus | Times | Div | Mod
    deriving (Show, Eq)

data Trig =  Sin  | Cos  | Tan deriving (Show, Eq, Read)


data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokTrig Trig 
           | TokIdent String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '%' = Mod
 
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/%" = TokOp (operator c) : tokenize cs
    | c == '='  = TokAssign : tokenize cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | isDigit c = number c cs
    | elem c "SCT" = keyword c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

keyword :: Char -> String -> [Token]
keyword c cs = let (rem,cs') = span isAlphaNum cs
                   (_,cs'') = span isSpace cs'
                in case (c:rem) of
                    theWord | elem theWord ["Sin","Cos","Tan"] -> TokTrig (read theWord) : tokenize cs''
                    _ -> TokIdent (c:rem) : tokenize cs''


    

identifier c cs = let (name, cs') = span isAlphaNum cs 
                   in TokIdent (c:name) : tokenize cs'

number :: Char -> String -> [Token]
number c cs = 
   let (digs, cs') = span (\x -> (isDigit x || x=='.')) cs in
   TokNum (read (c : digs)) : tokenize cs'

