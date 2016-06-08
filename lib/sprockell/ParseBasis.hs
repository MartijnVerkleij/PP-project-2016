{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

{- ===========================================================================
Contains basic types for Parsing - you'll have to extend several of the definitions below
=========================================================================== -}


module ParseBasis where

import GHC.Generics
import FPPrac.Trees

-- ===================================================================
-- Example Alphabet
-- - Extend, adapt, change the non-terminals to your own needs
-- - Do NOT change the first two groups of constructors (TermSymb ... Rep1)

data Alphabet = TermSymb String               -- Terminal symbol: WILL be included in parseTree
              | SyntCat  Alphabet             -- Checks whether a string belongs to a syntactic category

              | Alt   [Alphabet] [Alphabet]   -- Try both
              | Opt   [Alphabet]              -- Optional
              | Rep0  [Alphabet]              -- Zero or more repetitions
              | Rep1  [Alphabet]              -- One or more repetitions

              -- | <... define your own non-terminals ...>

              deriving (Eq,Ord,Show,Generic,ToRoseTree)

-- ===================================================================
-- Symbolic notation for EBNF constructors

ps <> qs = Alt  ps qs
(?:) ps  = Opt  ps
(*:) ps  = Rep0 ps
(+:) ps  = Rep1 ps

-- ===================================================================

type Grammar = Alphabet -> [[Alphabet]]

type Token   = (Alphabet,String)  -- Alphabet: indicates the "syntactic category" to which
                                  --      the String belongs (to distinguish, a.o., between
                                  --      reserved words and identifiers in general),
                                  -- String: the token itself,
				  -- NOTE: a token is a TWO-tuple, the number is added automaticallyy

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
                  , [(Int,Token)]  -- The remaining list of *indexed* input tokens
                  , [Alphabet]     -- List of non-terminals to check for left-recursiveness
                  )

-- ===================================================================
-- Pretty Printing

addSpace n = map ((replicate n ' ') ++)

addListNotation []                 =   [["["]]

addListNotation ([]:strss)         =   ["["]
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addListNotation ((str:strs):strss) =   (("["++str):strs)
                                     : [  (","++str'):strs' | (str':strs') <- strss ]

addEndBrack [strs]       = [ strs ++ ["]"] ]
addEndBrack (strs:strss) = strs : addEndBrack strss

class Prpr a where
  toStrings :: a -> [String]

  prpr :: a -> IO ()
  prpr = putStr . ('\n':) . (++"\n") . unlines . toStrings

  prprList :: [a] -> IO ()
  prprList = putStr . ('\n':) . unlines . concat . map (++[""]) . map toStrings


instance Prpr ParseTree where
  toStrings tree = case tree of
     PLeaf t                 -> ["PLeaf " ++ show t]
     PNode nt ts             -> ("PNode " ++ show nt) : (addSpace 7 $ concat $ addEndBrack $ addListNotation $ map toStrings ts)
                             where

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

instance Prpr RoseTree where
  toStrings (RoseNode str [RoseNode str' []]) = [str ++ " " ++ str']
  toStrings (RoseNode str ts)                 = str : (addSpace 4 $ concat $ map toStrings ts)
