module ASTBuilder where

import FPPrac.Trees
import Types

pTreeToAst :: ParseTree -> AST
pTreeToAst (PNode Program l)
    = ASTProgram (map pTreeToAst l) ([],[],[])
        where
            procAsts = map pTreeToAst l
            --(procs,stats) = span $ isProcedure l
            isProcedure (PNode Proc _) = True
            isProcedure _ = False
            --getPRecord = (\(x,y) -> (ASTProc x y _ _)) 

pTreeToAst (PNode Global (typ:var:[]))
    = ASTGlobal (getAlphabet (getStr typ)) (pTreeToAst var) Nothing ([],[],[])
pTreeToAst (PNode Global (typ:var:(PLeaf (Ass,_,_)):expr:[]))
    = ASTGlobal (getAlphabet (getStr typ)) (pTreeToAst var) (Just (pTreeToAst expr)) ([],[],[])
pTreeToAst (PNode Proc (pid:args_expr))
    = ASTProc (getStr pid) (makeAstArg $ init args_expr) expr ([],[],[])
        where
            expr = pTreeToAst $ last args_expr
            makeAstArg [] = []
            makeAstArg [x] = error "This Proc is incorrectly parsed"
            makeAstArg (x:y:xs) = (ASTArg (pTreeToAst x) (pTreeToAst y) ([],[],[])) : (makeAstArg xs)

pTreeToAst node@(PNode Type _)
    = ASTType (getStr node) ([],[],[])

pTreeToAst node@(PNode Var _)
    = ASTVar (getStr node) ([],[],[])

-- DeclStat (no assign) 
pTreeToAst (PNode Stat (typ@(PNode Type _):var:[]))
    = ASTDecl (getAlphabet (getStr typ)) (pTreeToAst var) Nothing ([],[],[])
-- DeclStat (assign)
pTreeToAst (PNode Stat (typ@(PNode Type _):var:(PLeaf (Ass,_,_)):expr:[]))
    = ASTDecl (getAlphabet (getStr typ)) (pTreeToAst var) (Just $ pTreeToAst expr) ([],[],[])
-- If statement
pTreeToAst (PNode Stat ((PLeaf (If,_,_)):expr:stat1:[]))
    = ASTIf (pTreeToAst expr) (pTreeToAst stat1) Nothing ([],[],[])
-- If statment with else
pTreeToAst (PNode Stat ((PLeaf (If,_,_)):expr:stat1:_:stat2:[]))
    = ASTIf (pTreeToAst expr) (pTreeToAst stat1) (Just $ pTreeToAst stat2) ([],[],[])
-- While statement
pTreeToAst (PNode Stat ((PLeaf (While,_,_)):expr:stat:[]))
    = ASTWhile (pTreeToAst expr) (pTreeToAst stat) ([],[],[])
-- Fork statement
pTreeToAst (PNode Stat ((PLeaf (Fork,_,_)):pid:args_expr))
    = ASTFork (getStr pid) (map pTreeToAst args_expr) ([],[],[])
-- Join statement
pTreeToAst (PNode Stat ((PLeaf (Join,_,_)):[]))
    = ASTJoin ([],[],[])
-- Call statement
pTreeToAst (PNode Stat (pid@(PNode Pid _):args_expr))
    = ASTCall (getStr pid) (map pTreeToAst args_expr) ([],[],[])
-- Expression statement
pTreeToAst (PNode Stat (expr@(PNode Expr _):[]))
    = pTreeToAst expr
-- Block statement
pTreeToAst (PNode Stat ((PLeaf (Brace,_,_)):stats))
    = ASTBlock (map pTreeToAst stats) ([],[],[])

-- Parentheses expression
pTreeToAst (PNode Expr (expr@(PNode Expr _):[]))
    = pTreeToAst expr
-- Assignment expression
pTreeToAst (PNode Expr (var:(PLeaf (Ass,_,_)):expr:[]))
    = ASTAss (pTreeToAst var) (pTreeToAst expr) ([],[],[])
-- Variable expression
pTreeToAst (PNode Expr (var@(PNode Var _):[]))
    = ASTVar (getStr var) ([],[],[])
-- IntType expression
pTreeToAst (PNode Expr (intType@(PNode IntType _):[]))
    = ASTInt (getStr intType) ([],[],[])
-- BoolType expression
pTreeToAst (PNode Expr (boolType@(PNode BoolType _):[]))
    = ASTBool (getStr boolType) ([],[],[])
-- Operation expression
pTreeToAst (PNode Expr (expr1:op@(PNode Op _):expr2:[]))
    = ASTOp (pTreeToAst expr1) (getStr op) (pTreeToAst expr2) ([],[],[])
-- Unary operation expression
pTreeToAst (PNode Expr (op@(PNode Unary _):expr:[]))
    = ASTUnary (getStr op) (pTreeToAst expr) ([],[],[])


astToRoseDebug :: AST -> RoseTree
astToRoseDebug (ASTProgram asts (f,g,v)) 
    = RoseNode ("program" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) (map astToRoseDebug asts)
astToRoseDebug (ASTGlobal typeStr ast Nothing (f,g,v))
    = RoseNode ("global " ++ (getTypeStr typeStr) ++ " -> " ++ (show f) ++ (show g) ++ (show v)) [(astToRoseDebug ast)]
astToRoseDebug (ASTGlobal typeStr ast1 (Just ast2) (f,g,v))
    = RoseNode ("global " ++ (getTypeStr typeStr) ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTProc str asts ast (f,g,v))
    = RoseNode ("procedure " ++ str ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug $ asts ++ [ast]
astToRoseDebug (ASTArg ast1 ast2 (f,g,v))
    = RoseNode ("arg" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTBlock asts (f,g,v))
    = RoseNode ("block" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug asts
astToRoseDebug (ASTDecl typeStr ast Nothing (f,g,v))
    = RoseNode (getTypeStr typeStr ++ " -> " ++ (show f) ++ (show g) ++ (show v)) [(astToRoseDebug ast)]
astToRoseDebug (ASTDecl typeStr ast1 (Just ast2) (f,g,v))
    = RoseNode (getTypeStr typeStr ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTIf ast1 ast2 Nothing (f,g,v))
    = RoseNode ("if" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTIf ast1 ast2 (Just ast3) (f,g,v))
    = RoseNode ("if" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2, ast3]
astToRoseDebug (ASTWhile ast1 ast2 (f,g,v))
    = RoseNode ("while" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTFork str asts (f,g,v))
    = RoseNode ("fork " ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug asts
astToRoseDebug (ASTJoin (f,g,v))
    = RoseNode ("join" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) []
astToRoseDebug (ASTCall str asts (f,g,v))
    = RoseNode ("call "++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug asts
astToRoseDebug (ASTAss ast1 ast2 (f,g,v))
    = RoseNode ("=" ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTVar str (f,g,v))
    = RoseNode (str ++ " -> " ++ (show f) ++ (show g) ++ (show v)) []
astToRoseDebug (ASTInt str (f,g,v))
    = RoseNode (str ++ " -> " ++ (show f) ++ (show g) ++ (show v)) []
astToRoseDebug (ASTBool str (f,g,v))
    = RoseNode (str ++ " -> " ++ (show f) ++ (show g) ++ (show v)) []
astToRoseDebug (ASTType str (f,g,v))
    = RoseNode (str ++ " -> " ++ (show f) ++ (show g) ++ (show v)) []
astToRoseDebug (ASTOp ast1 str ast2 (f,g,v))
    = RoseNode (str ++ " -> " ++ (show f) ++ (show g) ++ (show v)) $ map astToRoseDebug [ast1, ast2]
astToRoseDebug (ASTUnary str ast (f,g,v))
    = RoseNode (str ++ " -> " ++ (show f) ++ (show g) ++ (show v)) [(astToRoseDebug ast)]


astToRose :: AST -> RoseTree
astToRose (ASTProgram asts _) 
    = RoseNode "program" (map astToRose asts)
astToRose (ASTGlobal typeStr ast Nothing _)
    = RoseNode ("global " ++ (getTypeStr typeStr)) [(astToRose ast)]
astToRose (ASTGlobal typeStr ast1 (Just ast2) _)
    = RoseNode ("global " ++ (getTypeStr typeStr)) $ map astToRose [ast1, ast2]
astToRose (ASTProc str asts ast _)
    = RoseNode ("procedure " ++ str) $ map astToRose $ asts ++ [ast]
astToRose (ASTArg ast1 ast2 _)
    = RoseNode "arg" $ map astToRose [ast1, ast2]
astToRose (ASTBlock asts _)
    = RoseNode "block" $ map astToRose asts
astToRose (ASTDecl typeStr ast Nothing _)
    = RoseNode (getTypeStr typeStr) [(astToRose ast)]
astToRose (ASTDecl typeStr ast1 (Just ast2) _)
    = RoseNode (getTypeStr typeStr) $ map astToRose [ast1, ast2]
astToRose (ASTIf ast1 ast2 Nothing _)
    = RoseNode "if" $ map astToRose [ast1, ast2]
astToRose (ASTIf ast1 ast2 (Just ast3) _)
    = RoseNode "if" $ map astToRose [ast1, ast2, ast3]
astToRose (ASTWhile ast1 ast2 _)
    = RoseNode "while" $ map astToRose [ast1, ast2]
astToRose (ASTFork str asts _)
    = RoseNode ("fork " ++ str) $ map astToRose asts
astToRose (ASTJoin _)
    = RoseNode "join" []
astToRose (ASTCall str asts _)
    = RoseNode ("call " ++ str) $ map astToRose asts
astToRose (ASTAss ast1 ast2 _)
    = RoseNode "=" $ map astToRose [ast1, ast2]
astToRose (ASTVar str _)
    = RoseNode str []
astToRose (ASTInt str _)
    = RoseNode str []
astToRose (ASTBool str _)
    = RoseNode str []
astToRose (ASTType str _)
    = RoseNode str []
astToRose (ASTOp ast1 str ast2 _)
    = RoseNode str $ map astToRose [ast1, ast2]
astToRose (ASTUnary str ast _)
    = RoseNode str [(astToRose ast)]



getStr :: ParseTree -> String
getStr (PLeaf (_,str,_))    = str
getStr (PNode Var [x])      = getStr x
getStr (PNode Pid [x])      = getStr x
getStr (PNode BoolType [x]) = getStr x
getStr (PNode IntType [x])  = getStr x 
getStr (PNode Op [x])       = getStr x
getStr (PNode Unary [x])    = getStr x
getStr (PNode Type [x])     = getStr x
getStr (PNode Expr _)       = error "Cannot return the string of an expression this way."
getStr a                    = error (show a)

getAlphabet :: String -> Alphabet
getAlphabet "int"   = IntType
getAlphabet "bool"  = BoolType
getAlphabet _       = error "Type not recognised."

getTypeStr :: Alphabet -> String
getTypeStr IntType  = "int"
getTypeStr BoolType = "bool"
getTypeStr _        = error "Not a valid type in: getStr :: Alphabet -> String"
