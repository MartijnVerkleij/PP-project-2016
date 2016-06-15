

pTreeToAst :: ParseTree -> AST
pTreeToAst (PNode Program l)
    = ASTProgram (map pTreeToAst l) 
        where
            procAsts = map pTreeToAst procs
            --(procs,stats) = span $ isProcedure l
            isProcedure (PNode Proc _) = True
            isProcedure = False
            --getPRecord = (\(x,y) -> (ASTProc x y _ _)) 

pTreeToAst (PNode Proc (pid:args_expr))
    = ASTProc (getStr pid) (makeAstArg $ init args_expr) expr
        where
            expr = pTreeToAst $ tail args_expr
            makeAstArg [] = []
            makeAstArg [x] = error "This Proc is incorrectly parsed"
            makeAstArg (x:y:xs) = (ASTArg (pTreeToAst x) (pTreeToAst x)) : makeAstArg

pTreeToAst node@(PNode Type _)
    = ASTType (getStr node)

pTreeToAst node@(PNode Var _)
    = ASTVar (getStr node)

-- DeclStat (global, no assign)
pTreeToAst (PNode Stat ((PLeaf (Global,_,_)):typ@(PNode Type _):var:[]))
    = ASTDecl True (getType $ getStr typ) pTreeToAst var Nothing
-- DeclStat (no global, no assign) 
pTreeToAst (PNode Stat (typ@(PNode Type _):var:[]))
    = ASTDecl False (getType $ getStr typ) pTreeToAst var Nothing
-- DeclStat (global, assign)
pTreeToAst (PNode Stat ((PLeaf (Global,_,_)):typ@(PNode Type _):var:(PLeaf (Ass,_,_)):stat:[]) )
    = ASTDecl True (getType $ getStr typ) pTreeToAst var (Just $ pTreeToAst stat)
-- DeclStat (no global, assign)
pTreeToAst (PNode Stat (typ@(PNode Type _):var:(PLeaf (Ass,_,_):stat:[]))
    = ASTDecl False (getType $ getStr typ) pTreeToAst var (Just $ pTreeToAst stat)
-- If statement
pTreeToAst (PNode Stat ((PLeaf (If,_,_)):expr:stat1:[]))
    = ASTIf (pTreeToAst expr) (pTreeToAst stat1) Nothing
-- If statment with else
pTreeToAst (PNode Stat ((PLeaf (If,_,_)):expr:stat1:_:stat2:[]))
    = ASTIf (pTreeToAst expr) (pTreeToAst stat1) (Just $ pTreeToAst stat2)
-- While statement
pTreeToAst (PNode Stat ((PLeaf (While,_,_)):expr:stat:[]))
    = ASTWhile (pTreeToAst expr) (pTreeToAst stat)
-- Fork statement
pTreeToAst (PNode Stat ((PLeaf (Fork,_,_)):pid:args_expr))
    = ASTFork (getStr pid) (map (pTreeToAst) args_expr)
-- Join statement
pTreeToAst (PNode Stat ((PLeaf (Join,_,_)):[]))
    = ASTJoin
-- Call statement
pTreeToAst (PNode Stat (pid@(PNode Pid _):args_expr))
    = ASTCall (getStr pid) (map (pTreeToAst) args_expr)
-- Expression statement
pTreeToAst (PNode Stat (expr@(PNode Expr _)))
    = pTreeToAst expr
-- Block statement
pTreeToAst (PNode Stat ((PLeaf (Brace,_,_)):stats))
    = ASTBlock (map (pTreeToAst) stats)

-- Parentheses expression
pTreeToAst (PNode Expr expr@(PNode Expr _))
    = pTreeToAst expr
-- Assignment expression
pTreeToAst (PNode Expr (var:(PLeaf (Ass,_,_)):expr:[]))
    = ASTAss (getStr var) (pTreeToAst expr)
-- Variable expression
pTreeToAst (PNode Expr var@(PNode Var _))
    = ASTVar (getStr var)
-- IntType expression
pTreeToAst (PNode Expr intType@(PNode IntType _))
    = ASTInt (getStr intType)
-- BoolType expression
pTreeToAst (PNode Expr boolType@(PNode BoolType _))
    = ASTBool (getStr boolType)
-- Operation expression
pTreeToAst (PNode Expr ((expr1:op@(PNode Op _):expr2:[]))
    = ASTOp (pTreeToAst expr1) (getStr op) (pTreeToAst expr2)
-- Unary operation expression
pTreeToAst (PNode Expr (op@(PNode Unary _):expr:[]))
    = ASTUnary (getStr op) (pTreeToAst expr)




pTreeToAst (PNode Pid _) 
    = error "No AST for Pid"

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



getStr :: AST -> String
getStr (PLeaf (_,str,_))    = str
getStr (PNode Var x)        = getStr x
getStr (PNode Pid x)        = getStr x
getStr (PNode BoolType x)   = getStr x
getStr (PNode IntType x)    = getStr x 
getStr (PNode Op x)         = getStr x
getStr (PNode Unary x)      = getStr x
getStr (PNode Expr _)       = error "Cannot return the string of an expression this way."

getAlphabet :: String -> Alphabet
getAlphabet "int" = IntType
getAlphabet "bool" = BoolType
getAlphabet _ = error "Type not recognised."
