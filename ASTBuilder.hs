

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
pTreeToAst (PNode Stat ((PLeaf (Global,_,_)):typ@(PNode Type _):var:[]) )
-- DeclStat (no global, no assign)
pTreeToAst (PNode Stat (typ@(PNode Type _):var:[]))
-- DeclStat (global, assign)
pTreeToAst (PNode Stat ((PLeaf (Global,_,_)):typ@(PNode Type _):var:(PLeaf (Ass,_,_):stat:[]) )
-- DeclStat (no global, assign)
pTreeToAst (PNode Stat (typ@(PNode Type _):var:(PLeaf (Ass,_,_):stat:[]))


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
getStr (PNode Expr _)       = error "Cannot return the string of an expression this way."