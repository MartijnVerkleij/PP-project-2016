{-

    Declarations have a type.
    
    Statements are ended with a semicolon.
    
 -- Standard language features --
    
    We support procedures:
    
    procedure Pid ([TYPE PVAR1, ... , TYPE PVARN]) STAT
    
    We support the following statements:
    
    if (EXPR) STAT [else STAT] ;
    while (EXPR) STAT ;
    TYPE VAR [= EXPR] ;
    Pid ([PVAR1, ... , PVARN]) ;        -- if PVAR is a naked variable, it is passed call-by-reference
    
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
    
    fork Pid ([PVAR1, ... , PVARN]) ;   -- if PVAR is a naked variable, it is passed call-by-reference
    join ;
    



-}

{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
        -- Necessary for function toRoseTree

module Grammar where

{- ===========================================================================
Contains example grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

import FPPrac.Trees         -- Contains now also the function toRoseTree. Re-install it!
import GHC.Generics         -- Necessary for correct function of FPPrac

import Types                -- Extend the file TypesEtc with your own alphabet
import FP_ParserGen (parse) -- Touching this file leaves you at your own devices
-- import Tokenizer         -- You'll have to write a file for tokenizing yourself

-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function
--      (where the Alphabet is in the file TypesEtc.hs)

grammar :: Grammar

grammar nt = case nt of

        -- Program

        Program     ->  [[ (*:) [Global], (*:) [Proc], (*:) [Stat] ]]

        -- Globals

        Global      ->  [[ global, Type, Var, (?:) [ass, Expr], eol ]]

        -- Procedures

        Proc        ->  [[ procedure, Pid, lPar, (?:) [Type, Var, (*:) [comma, Type, Var]], rPar, Stat ]]

        -- Statements

        Stat        ->  [[ Type, Var, (?:) [ass, Expr], eol ]                               -- declaration
                        ,[ ifStr, lPar, Expr, rPar, Stat, (?:) [elseStr, Stat] ]            -- if
                        ,[ while, lPar, Expr, rPar, Stat ]                                  -- while
                        ,[ fork, Pid, lPar, (?:) [Expr, (*:) [comma, Expr]], rPar, eol ]    -- fork
                        ,[ join, eol ]                                                      -- join
                        ,[ Pid, lPar, (?:) [Expr, (*:) [comma, Expr]], rPar, eol ]          -- call
                        ,[ Expr, eol ]                                                      -- expression
                        ,[ lBrace, (*:) [Stat], rBrace ]]                                   -- block
        -- Expressions

        Expr        ->  [[ lPar, Expr, rPar ]               -- parentheses
                        ,[ Var, ass, Expr ]                 -- assignment
                        ,[ Var ]                            -- variable
                        ,[ IntType ]                        -- integer
                        ,[ BoolType ]                       -- boolean
                        ,[ lPar, Expr, Op, Expr, rPar ]     -- operation
                        ,[ Unary, Expr ]]                   -- unary operation

        -- Other

        Type        ->  [[ typeStr ]]       -- type

        Var         ->  [[ var ]]           -- variable

        Pid         ->  [[ Var ]]           -- procedure identifier

        IntType     ->  [[ intType ]]       -- number

        BoolType    ->  [[ boolType ]]      -- boolean

        Op          ->  [[ op ]]            -- operator

        Unary       ->  [[ Op ]]         -- unary operator


-- shorthand names can be handy, such as:
lPar        = Symbol "("           -- Terminals WILL be shown in the parse tree
rPar        = Symbol ")"
lBrace      = Terminal "{"
rBrace      = Symbol "}"
procedure   = Symbol "procedure"
ifStr       = Terminal "if"
elseStr     = Terminal "else"
while       = Terminal "while"
ass         = Terminal "="
fork        = Terminal "fork"
join        = Terminal "join"
global      = Symbol "global"


eol         = Symbol ";"
comma       = Symbol ","

-- alternative:
-- lBracket  = Symbol "("          -- Symbols will NOT be shown in the parse tree.
-- rBracket  = Symbol ")"

var         = SyntCat Var
--pid         = SyntCat Pid
intType     = SyntCat IntType
boolType    = SyntCat BoolType
op          = SyntCat Op
unary       = SyntCat Unary
typeStr     = SyntCat Type



-- ==========================================================================================================
-- TESTING: example expression: "((10+20)*30)"

-- Result of tokenizer (to write yourself) should be something like:
{-tokenList0 = [ (Bracket,"(",0)
             , (Bracket,"(",1)
             , (Nmbr,"10",2)
             , (Op,"+",3)
             , (Nmbr,"20",4)
             , (Bracket,")",5)
             , (Op,"*",6)
             , (Nmbr,"30",7)
             , (Bracket,")",8)
             ]-}

-- Parse this tokenlist with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList0: the tokenlist above
--parseTree0 = parse grammar Expr tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
--testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
--testGr     = showTree $ toRoseTree parseTree0
