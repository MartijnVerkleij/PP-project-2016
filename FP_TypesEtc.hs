{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

{- ===========================================================================
Contains basic types - you'll have to extend several of the definitions below
=========================================================================== -}


module FP_TypesEtc where

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

              | Expr                          -- Expression
              | Nmbr                          -- Number
              | Var                           -- Variable (self-defined)
              | Op                            -- Operation symbol
              | Space                         -- Spaces
              | Bracket                       -- Brackets
              | Brace                         -- Braces
              | Rsvd                          -- Reserved word (self-defined)
              
              | Program                       -- Program (self-defined)
              | Stat                          -- Statements (self-defined)
              | Block                         -- Block statement (self-defined)
              | Asm                           -- Assignment (self-defined)
              | Rep                           -- Repeat (self-defined)
              | If                            -- If (self-defined)
              | Then                          -- Then (self-defined)
              | Else                          -- Else (self-defined)
              deriving (Eq,Ord,Show,Generic,ToRoseTree)

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

--------------- Own stuff ----------------------

resWord :: [Token] -> [Token]
resWord = map resWord'
    where
        resWord' (Var,b,c)  | elem b res = (Rsvd,b,c)
                            | otherwise  = (Var,b,c)
            where
                res = ["while","if","then","repeat","else"]
        resWord' x          = x

data AST = ASTRoot [AST]
         | ASTStat String AST [AST] [AST]
         | ASTExpr String [AST]
         | ASTBlock [AST]
         | ASTLeaf
            deriving Show
         
ptreeToAst :: ParseTree -> AST
ptreeToAst (PNode Program l)
    = ASTRoot (map ptreeToAst l)
ptreeToAst (PNode Stat (st@(PLeaf (Rep,_,_)):e:bl:_))
    = ASTStat "repeat" (ptreeToAst e) [(ptreeToAst bl)] []
ptreeToAst (PNode Stat (st@(PLeaf (If,_,_)):e:(PLeaf (Then,_,_)):th:(PLeaf (Else,_,_)):el:_))
    = ASTStat "if" (ptreeToAst e) [(ptreeToAst th)] [(ptreeToAst el)]
ptreeToAst (PNode Stat (st@(PLeaf (If,_,_)):e:(PLeaf (Then,_,_)):th:_))
    = ASTStat "if" (ptreeToAst e) [(ptreeToAst th)] []
ptreeToAst (PNode Block l)
    = ASTBlock (map ptreeToAst l)
ptreeToAst (PNode Stat ((PNode Var [PLeaf (_,v,_)]):(PLeaf (Asm,_,_)):e:_) )
    = ASTStat (v ++ " := ") (ptreeToAst e) [] []
ptreeToAst (PNode Expr ((PLeaf (Bracket,_,_)):f:(PNode Op [PLeaf (_,o,_)]):s:_))
    = ASTExpr o [ptreeToAst f, ptreeToAst s]
ptreeToAst (PNode Expr [(PNode Nmbr [PLeaf (_,n,_)])])
    = ASTExpr n []
ptreeToAst (PNode Expr [(PNode Var [PLeaf (_,n,_)])])
    = ASTExpr n []
ptreeToAst e
    = ASTLeaf

{-
ptreeToAst (PNode Program l) = ASTRoot (map ptreeToAst $ tail $ init l)
ptreeToAst (PNode Stat ((PLeaf (Rep,_,_)):e:_:ss) )
    = ASTStat "repeat" (ptreeToAst e) (map ptreeToAst $ init ss) []
ptreeToAst (PNode Stat ((PLeaf (If,_,_)):e:(PLeaf (Then,_,_)):(PLeaf (Bracket,_,_)):th:(PLeaf (Bracket,_,_)):(PLeaf (Else,_,_)):(PLeaf (Bracket,_,_)):el:(PLeaf (Bracket,_,_)):_) )
    = ASTStat "if" (ptreeToAst e) (map ptreeToAst th) (map ptreeToAst el)
ptreeToAst (PNode Stat ((PLeaf (If,_,_)):e:(PLeaf (Then,_,_)):(PLeaf (Bracket,_,_)):th:(PLeaf (Bracket,_,_)):_))
    = ASTStat "if" (ptreeToAst e) (map ptreeToAst th) []
ptreeToAst (PNode Stat ((PNode Var [PLeaf (_,v,_)]):(PLeaf (Asm,_,_)):e) )
    = ASTStat (v ++ " := ") (ptreeToAst $ head e) [] []
ptreeToAst (PNode Expr ((PLeaf (Bracket,_,_)):f:(PNode Op [PLeaf (_,o,_)]):s:_))
    = ASTExpr o [ptreeToAst f, ptreeToAst s]
ptreeToAst (PNode Expr [(PNode Nmbr [PLeaf (_,n,_)])])
    = ASTExpr n []
ptreeToAst (PNode Expr [(PNode Var [PLeaf (_,n,_)])])
    = ASTExpr n []
ptreeToAst e
    = error $ show e
-}

astToRose :: AST -> RoseTree
astToRose (ASTRoot e) = RoseNode "program" (map astToRose e)
astToRose (ASTBlock e) = RoseNode "block" (map astToRose $ init $ tail e)
astToRose (ASTStat str expr stat1 stat2) = RoseNode str buildlist
    where
        buildlist   | length stat1 > 0 && (length stat2 > 0)    = [astToRose expr, RoseNode "" (map astToRose stat1), RoseNode "" (map astToRose stat2)]
                    | length stat1 > 0                          = [astToRose expr, RoseNode "" (map astToRose stat1)]
                    | length stat2 > 0                          = [astToRose expr, RoseNode "" (map astToRose stat2)]
                    | otherwise                                 = [astToRose expr]
astToRose (ASTExpr str []) = RoseNode str []
astToRose (ASTExpr str exprs) = RoseNode str (map astToRose exprs)
astToRose (ASTLeaf) = RoseNode "" []
