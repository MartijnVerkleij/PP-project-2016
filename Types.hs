{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Types where

{- ===========================================================================
Contains basic types - you'll have to extend several of the definitions below
=========================================================================== -}

import GHC.Generics
import FPPrac.Trees

-- ===================================================================
-- Example Alphabet
-- - Extend, adapt, change the non-terminals to your own needs
-- - Do NOT change the first two groups of constructors (Symbol ... Rep1)

data Alphabet = Terminal String               -- Terminal symbol: WILL be included in parseTree
              | Symbol   String               -- Terminal symbol: will NOT be included in parseTree
              | SyntCat  Alphabet             -- Checks whether a string belongs to a syntactic category

              | Alt   [Alphabet] [Alphabet]   -- Try both
              | Opt   [Alphabet]              -- Optional
              | Rep0  [Alphabet]              -- Zero or more repetitions
              | Rep1  [Alphabet]              -- One or more repetitions

              -- Types
              | IntType                       -- Integer value
              | BoolType                      -- Boolean value
              
              | Var                           -- Variable name
              | Pid                           -- Procedure name
              | Type                          -- Type
              
              
              -- punctuation
              | Op                            -- Infix operator
              | Unary                         -- Unary (for now prefix) operator
              | Par                           -- Parentheses
              | Brace                         -- Braces
              | Comma                         -- Comma
              | Semi                          -- Semicolon
              
              -- expressions
              | Expr                          -- Expression 
              
              | Ass                           -- Assignment espression
              -- Statements
              | Stat                          -- Statement
              
              | Decl                          -- Declaration statement
              | If                            -- If statement
              | Else                          -- If statement
              | While                         -- While statement
              | Call                          -- Procedure call
              | Block                         -- Block statement
              
              | Fork                          -- Fork statement
              | Join                          -- Join statement
              | Global                        -- Global keyword
              | Print
              
              | Program                       -- Program
              | Proc                          -- Procedure
                deriving (Eq,Ord,Show,Generic,ToRoseTree)

--type TypeTable = [(String, [AST{-Var-}])]
type VariableType = (String, Alphabet{- Must be of either IntType or BoolType -})
type FunctionType = (String, [(String, Alphabet{- Must be of either IntType or BoolType -})])


-- CheckType [Functions] [Globals] [Scopes[Variables]]
-- Scopes are defined in the following manner: [deepest scope,...,shallowest scope]
type CheckType = ([FunctionType], [VariableType], [[VariableType]])

-- An Alphabet, in this context, is equivalent to a type
data AST = ASTProgram [AST] CheckType
    -- Globals
    | ASTGlobal Alphabet AST (Maybe AST) CheckType
    -- Procedures and arguments
    | ASTProc String [AST] AST CheckType
    | ASTArg AST AST CheckType
    -- Statements
    | ASTBlock [AST] CheckType
    | ASTDecl Alphabet AST (Maybe AST) CheckType
    | ASTIf AST AST (Maybe AST) CheckType
    | ASTWhile AST AST CheckType
    | ASTFork String [AST] CheckType
    | ASTJoin CheckType
    | ASTCall String [AST] CheckType
    | ASTPrint [AST] CheckType
    -- Expressions
    | ASTExpr AST (Maybe Alphabet) CheckType
    | ASTAss AST AST (Maybe Alphabet) CheckType
    | ASTVar String CheckType
    | ASTInt String CheckType
    | ASTBool String CheckType
    | ASTType String CheckType
    | ASTOp AST String AST (Maybe Alphabet) CheckType
    | ASTUnary String AST (Maybe Alphabet) CheckType
    deriving Show



-- ===================================================================
-- Symbolic notation for EBNF constructors

ps <> qs = Alt  ps qs
(?:) ps  = Opt  ps
(*:) ps  = Rep0 ps
(+:) ps  = Rep1 ps

-- ===================================================================

type Grammar = Alphabet -> [[Alphabet]]

type Token   = (Alphabet,String,Int)  -- Alphabet: indicates the "syntactic category" to which
                                      --      the String belongs (to distinguish, a.o., between
                                      --      reserved words and identifiers in general),
                                      -- String: the token itself,
                                      -- Int: the position of the token in the input token-list
                                      --      (needed for error messages).

instance ToRoseTree Token where
  toRoseTree t = RoseNode (show t) []

data ParseTree  = PLeaf Token
                | PNode Alphabet [ParseTree]
                | PError ParseTree [Alphabet] Alphabet String Int
                deriving (Eq,Show,Generic,ToRoseTree)

instance Ord ParseTree where
  PError _ _ _ _ k <  PError _ _ _ _ k' = k <  k'
  _                <  _                 = error "ordering only in case of parse-errors"

  PError _ _ _ _ k <= PError _ _ _ _ k' = k <= k'
  _                <= _                 = error "ordering only in case of parse-errors"

type ParseState = ( Alphabet       -- Non-terminal indicating the present subexpression
                  , [ParseTree]    -- The already produced trees within the present subexpression
                  , [Token]        -- The remaining list of input tokens
                  )

-- ===================================================================
x ∈ xs = x `elem` xs

-- ===================================================================
-- Pretty Printing

toStrings tree = case tree of
     PLeaf t                 -> ["PLeaf " ++ show t]

     PNode nt ts             -> ("PNode " ++ show nt) : (addSpace 7 $ concat $ addEndBrack $ addListNotation $ map toStrings ts)
                             where
                               addSpace n = map ((replicate n ' ') ++)

                               addListNotation ((str:strs):strss) =   (("["++str):strs)
                                                                    : [  (","++str'):strs' | (str':strs') <- strss ]

                               addEndBrack [strs]       = [ strs ++ ["]"] ]
                               addEndBrack (strs:strss) = strs : addEndBrack strss 

     PError tr rule nt str k -> [ "==========="
                                , "Parse Error"
                                , "==========="
                                , "Recognized:"
                                , "-----------"
                                ]
                                ++ toStrings tr ++
                                [ "-----------"
                                , "Still to go:   " ++ show rule
                                , "Expected:      " ++ show nt
                                , "Found:         " ++ str
                                , "At position:   " ++ show k
                                , "==========="
                                ]

prpr t  = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings t

