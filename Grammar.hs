{-

    Declarations have a type.
    
    Statements are ended with a semicolon.
    
 -- Standard language features --
    
    We support procedures:
    
    procedure PID ([TYPE PVAR1, ... , TYPE PVARN]) STAT
    
    We support the following statements:
    
    if (EXPR) STAT [else STAT] ;
    while (EXPR) STAT ;
    TYPE VAR [= EXPR] ;
    PID ([PVAR1, ... , PVARN]) ;        -- if PVAR is a naked variable, it is passed call-by-reference
    
    We use blocks to define groups of statements. These are also used in scoping.
    
    We support the following expressions:
    
    VAR = EXPR -> EXPR
    BOOL < ==,!=,&&,||,<> > BOOL -> BOOL
    ! BOOL -> BOOL
    INT < +,-,*,/,^,% > INT -> INT
    INT < <,>,<=,>=,==,!= > INT -> BOOL
    
    
 -- Concurrent features --
    
    We support the following statements:
    
    global TYPE VAR [= (EXPR)] ;
    
    fork PID ([PVAR1, ... , PVARN]) ;   -- if PVAR is a naked variable, it is passed call-by-reference
    join ;
    



-}

{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
        -- Necessary for function toRoseTree

module Grammar where

{- ===========================================================================
Contains example grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

import FPPrac.Trees       -- Contains now also the function toRoseTree. Re-install it!
import GHC.Generics       -- Necessary for correct function of FPPrac

import Types           -- Extend the file TypesEtc with your own alphabet
import FP_ParserGen (parse)  -- Touching this file leaves you at your own devices
-- import Tokenizer       -- You'll have to write a file for tokenizing yourself

-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function
--      (where the Alphabet is in the file TypesEtc.hs)

grammar :: Grammar

grammar nt = case nt of

        -- Program

        Program ->  [[ (*:) [Proc], (*:) [Stat] ]]

        -- Procedures

        Proc    ->  [[ procedure, PID, lPar, (?:) [Type, Var, (*:) [comma, Type, Expr]], rPar, Stat ]]

        -- Statements

        Stat    ->  [[ Decl ]
                    ,[ If ]
                    ,[ While ]
                    ,[ Fork ]
                    ,[ Join ]
                    ,[ Call ]
                    ,[ Block ]]

        Decl    ->  [[ (?:) [global], Type, Var, (?:) [ass, Expr], eol ]]

        If      ->  [[ ifStr, lPar, Expr, rPar, Stat, (?:) [elseStr, Stat], eol ]]

        While   ->  [[ while, lPar, Expr, rPar, Stat, eol ]]

        Fork    ->  [[ fork, PID, lPar, (?:) [Expr, (*:) [comma, Expr]], rPar, eol ]]

        Join    ->  [[ join, eol ]]

        Call    ->  [[ PID, lPar, (?:) [Expr, (*:) [comma, Expr]], rPar, eol ]]

        Block   ->  [[ rBrace, (*:) [Stat], lBrace ]]

        -- Expressions

        Expr    ->  [[ lPar, Expr, rPar ]
                    ,[ Ass ]
                    ,[ Expr, Op, Expr ]
                    ,[ Unary, Expr ]]

        Ass     ->  [[ Var, ass, Expr ]]

        -- Other

        Type    ->  [[ typeStr ]]

        Var     ->  [[ var ]]

        PID     ->  [[ pid ]]

        Nmbr    ->  [[ nmbr ]]

        Op      ->  [[ op ]]

        Unary   ->  [[ unary ]]


-- shorthand names can be handy, such as:
lPar        = Terminal "("           -- Terminals WILL be shown in the parse tree
rPar        = Terminal ")"
lBrace      = Terminal "{"
rBrace      = Terminal "}"
procedure   = Terminal "procedure"
ifStr       = Terminal "if"
elseStr     = Terminal "else"
while       = Terminal "while"
ass         = Terminal "="
fork        = Terminal "fork"
join        = Terminal "join"


eol         = Symbol ";"
comma       = Symbol ","

-- alternative:
-- lBracket  = Symbol "("          -- Symbols will NOT be shown in the parse tree.
-- rBracket  = Symbol ")"

var     = SyntCat Var
pid     = SyntCat PID
nmbr    = SyntCat Nmbr
op      = SyntCat Op
unary   = SyntCat Unary
typeStr = SyntCat Type



-- ==========================================================================================================
-- TESTING: example expression: "((10+20)*30)"

-- Result of tokenizer (to write yourself) should be something like:
tokenList0 = [ (Bracket,"(",0)
             , (Bracket,"(",1)
             , (Nmbr,"10",2)
             , (Op,"+",3)
             , (Nmbr,"20",4)
             , (Bracket,")",5)
             , (Op,"*",6)
             , (Nmbr,"30",7)
             , (Bracket,")",8)
             ]

-- Parse this tokenlist with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList0: the tokenlist above
parseTree0 = parse grammar Expr tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
testGr     = showTree $ toRoseTree parseTree0
