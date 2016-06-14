

pTreeToAst :: ParseTree -> AST
pTreeToAst (PNode Program l)
    = ASTProgram (map pTreeToAst l) []
        where 
            procAsts = map pTreeToAst procs
            --(procs,stats) = span $ isProcedure l
            isProcedure (PNode Proc _) = True
            isProcedure = False
            --getPRecord = (\(x,y) -> (ASTProc x y _ _)) 
            
pTreeToAst (PNode Proc (pid:args_expr))
    = ASTProc (getstr pid) 
        where
            expr  
            
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