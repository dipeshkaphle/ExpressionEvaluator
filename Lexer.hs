module Lexer where
import Data.Char

data Operator = Plus | Minus | Times | Div | Mod
    deriving (Show, Eq)

data Trig =  Sin  | Cos  | Tan deriving (Show, Eq, Read)


data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokTrig Trig Token
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
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

-- Added support for trig identities 
identifier :: Char -> String -> [Token]
identifier c cs = case c of
    firstChar | elem firstChar "SCT" -> let (rem,cs') = span isAlphaNum cs  -- Nesting level 1
                                            (_,cs'') = span isSpace  cs'
                    in
                        case (c:rem) of -- Nesting level 2j 
                            theWord | elem theWord ["Sin", "Cos","Tan"] -> case (head cs'') of
                                firstLetter | isDigit firstLetter -> let (digs ,cs''') = span (\x -> (isDigit x || x == '.')) (tail cs'')--Nest3
                                                                      in TokTrig (read theWord) (TokNum (read (firstLetter:digs))) : tokenize cs'''
                                _ -> let (x:xs) = identifier (head cs'') (tail cs'') --Nest3
                                      in TokTrig (read theWord) (x) : xs
                            _ -> TokIdent (c:rem) : tokenize cs' --Nesting 2
    _ -> let (name ,cs') = span isAlphaNum cs -- Nesting 1
          in TokIdent (c:name) : tokenize cs'
                               
-- This was enough if i hadnt added trig support
    {- identifier c cs = let (name, cs') = span isAlphaNum cs in
                  TokIdent (c:name) : tokenize cs' -}

number :: Char -> String -> [Token]
number c cs = 
   let (digs, cs') = span (\x -> (isDigit x || x=='.')) cs in
   TokNum (read (c : digs)) : tokenize cs'

