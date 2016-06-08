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


