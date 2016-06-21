module Tokenizer where

import Grammar
import Types
import Data.List
import Data.Char
import FP_ParserGen         -- Touching this file leaves you at your own devices
import Debug.Trace


tokenizer :: String -> [(Alphabet,String)]
tokenizer [] = []

tokenizer (';':xs)  = (Semi, ";") : tokenizer xs
tokenizer (',':xs)  = (Comma, ",") : tokenizer xs

tokenizer input@(x:xs) 
    | isSpace x             = tokenizer xs
    | elem x "()"           = (Par, [x]) : (tokenizer xs)
    | elem x "{}"           = (Brace, [x]) : (tokenizer xs)
    | word == "procedure"   = (Proc, word) : (tokenizer wordRest)
    | word == "if"          = (If, word) : (tokenizer wordRest)
    | word == "else"        = (Else, word) : (tokenizer wordRest)
    | word == "while"       = (While, word) : (tokenizer wordRest)
    | word == "fork"        = (Fork, word) : (tokenizer wordRest)
    | word == "join"        = (Join, word) : (tokenizer wordRest)
    | word == "global"      = (Global, word) : (tokenizer wordRest)
    | word == "true"        = (BoolType, word) : (tokenizer wordRest)
    | word == "false"       = (BoolType, word) : (tokenizer wordRest)
    | word == "blackjack"   = (BoolType, word) : (tokenizer wordRest)
    | word == "hookers"     = (BoolType, word) : (tokenizer wordRest)
    | word == "int"         = (Type, word) : (tokenizer wordRest)
    | word == "bool"        = (Type, word) : (tokenizer wordRest)
    
    | isPrefixOf "==" input = (Op, "==") : tokenizer (input \\ "==")
    | isPrefixOf "!=" input = (Op, "!=") : tokenizer (input \\ "!=")
    | isPrefixOf "&&" input = (Op, "&&") : tokenizer (input \\ "&&")
    | isPrefixOf "||" input = (Op, "||") : tokenizer (input \\ "||")
    | isPrefixOf "<>" input = (Op, "<>") : tokenizer (input \\ "<>")
    | isPrefixOf "<=" input = (Op, "<=") : tokenizer (input \\ "<=")
    | isPrefixOf ">=" input = (Op, ">=") : tokenizer (input \\ ">=")

    | isPrefixOf "//" input = tokenizer $ endOfLine (input \\ "//")
    | isPrefixOf "/*" input = tokenizer $ endOfBlock (input \\ "/*")
    
    | elem x "!+-^*/%<>"    = (Op, [x]) : tokenizer xs
    
    | x ==  '='              = (Ass, [x]) : tokenizer xs
    
    
    | int /= ""             = (IntType, int) : tokenizer intRest
    | word /= ""            = (Var, word) : tokenizer wordRest
    | otherwise             = error ("Lexical error at character \"" ++ [x] ++ 
                                  "\", left to parse: \"" ++ xs ++ "\"" )
        where 
            (word,wordRest)     = span isAlphaNum input
            (int,intRest)       = span isNumber input
            endOfLine :: String -> String
            endOfLine []        = []
            endOfLine (x:xs)    | x `elem` "\r\n\f" = xs
                                | otherwise         = endOfLine xs
            endOfBlock :: String -> String
            endOfBlock []       = []
            endOfBlock (x:y:xs) | [x,y] == "*/"     = xs
                                | otherwise         = endOfBlock (y:xs)

-- From series6a
toTokenList :: [(Alphabet, String)] -> [Token]
toTokenList tl = zipWith (\ (x,y) z -> (x,y,z) ) tl [0..]
