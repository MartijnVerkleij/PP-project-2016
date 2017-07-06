import Sprockell

main = run [prog]

prog =
  [ Load (ImmValue 1) 6
  , WriteInstr 6 (IndAddr 0)
  , ReadInstr (IndAddr 0)
  , Receive 6
  , Compute Equal 0 1 6
  , Branch 6 (Rel 11)
  , ReadInstr (IndAddr 1)
  , Receive 5
  , Load (ImmValue 0) 6
  , Compute NEq 5 6 6
  , Branch 6 (Ind 5)
  , ReadInstr (IndAddr 0)
  , Receive 4
  , Compute Gt 4 0 4
  , Branch 4 (Rel (-8))
  , Jump (Rel 278)
  , Load (ImmValue 10) 6
  , Store 6 (DirAddr 1)
  , Load (ImmValue 32) 6
  , Store 6 (DirAddr 2)
  , Load (ImmValue 4) 5
  , Store 5 (DirAddr 4)
  , Load (ImmValue 49) 6
  , Store 6 (DirAddr 5)
  , Load (ImmValue 50) 6
  , Store 6 (DirAddr 6)
  , Load (ImmValue 51) 6
  , Store 6 (DirAddr 7)
  , Load (ImmValue 52) 6
  , Store 6 (DirAddr 8)
  , Load (ImmValue 4) 6
  , Store 6 (DirAddr 3)
  , Load (ImmValue 9608) 6
  , Push 6
  , Load (ImmValue 1) 4
  , Load (DirAddr 3) 6
  , Push 6
  , Load (ImmValue 0) 6
  , Pop 5
  , Compute Add 5 4 5
  , Compute Add 5 6 6
  , Pop 5
  , Store 5 (IndAddr 6)
  , Load (ImmValue 9619) 6
  , Push 6
  , Load (ImmValue 1) 4
  , Load (DirAddr 3) 6
  , Push 6
  , Load (ImmValue 1) 6
  , Pop 5
  , Compute Add 5 4 5
  , Compute Add 5 6 6
  , Pop 5
  , Store 5 (IndAddr 6)
  , Load (ImmValue 9618) 6
  , Push 6
  , Load (ImmValue 1) 4
  , Load (DirAddr 3) 6
  , Push 6
  , Load (ImmValue 2) 6
  , Pop 5
  , Compute Add 5 4 5
  , Compute Add 5 6 6
  , Pop 5
  , Store 5 (IndAddr 6)
  , Load (ImmValue 9617) 6
  , Push 6
  , Load (ImmValue 1) 4
  , Load (DirAddr 3) 6
  , Push 6
  , Load (ImmValue 3) 6
  , Pop 5
  , Compute Add 5 4 5
  , Compute Add 5 6 6
  , Pop 5
  , Store 5 (IndAddr 6)
  , Load (ImmValue 23) 6
  , Store 6 (DirAddr 9)
  , Load (ImmValue 23) 6
  , Push 6
  , Load (ImmValue 7) 6
  , Pop 5
  , Compute Sub 5 6 6
  , Store 6 (DirAddr 10)
  , Load (ImmValue 1) 6
  , Push 6
  , Load (DirAddr 9) 6
  , Pop 5
  , Compute LShift 5 6 6
  , Store 6 (DirAddr 11)
  , Load (ImmValue 344148) 6
  , Store 6 (DirAddr 12)
  , Load (ImmValue 440401) 6
  , Store 6 (DirAddr 13)
  , Load (ImmValue 134) 6
  , Load (ImmValue (-1)) 5
  , Compute Mul 6 5 6
  , Push 6
  , Load (DirAddr 10) 6
  , Pop 5
  , Compute LShift 5 6 6
  , Store 6 (DirAddr 14)
  , Load (ImmValue 282) 6
  , Load (ImmValue (-1)) 5
  , Compute Mul 6 5 6
  , Push 6
  , Load (DirAddr 10) 6
  , Pop 5
  , Compute LShift 5 6 6
  , Store 6 (DirAddr 15)
  , Load (DirAddr 15) 6
  , Store 6 (DirAddr 16)
  , Load (DirAddr 14) 6
  , Push 6
  , Load (ImmValue 134) 6
  , Push 6
  , Load (DirAddr 10) 6
  , Pop 5
  , Compute LShift 5 6 6
  , Pop 5
  , Compute Lt 5 6 6
  , Load (ImmValue 1) 5
  , Compute Xor 6 5 6
  , Branch 6 (Rel 167)
  , Load (DirAddr 16) 6
  , Store 6 (DirAddr 15)
  , Load (DirAddr 14) 6
  , Push 6
  , Load (DirAddr 13) 6
  , Pop 5
  , Compute Add 5 6 6
  , Store 6 (DirAddr 14)
  , Load (DirAddr 15) 6
  , Push 6
  , Load (DirAddr 11) 6
  , Pop 5
  , Compute Lt 5 6 6
  , Load (ImmValue 1) 5
  , Compute Xor 6 5 6
  , Branch 6 (Rel 148)
  , Load (DirAddr 15) 6
  , Push 6
  , Load (DirAddr 12) 6
  , Pop 5
  , Compute Add 5 6 6
  , Store 6 (DirAddr 15)
  , Load (ImmValue 0) 6
  , Store 6 (DirAddr 17)
  , Load (ImmValue 0) 6
  , Store 6 (DirAddr 18)
  , Load (ImmValue 0) 6
  , Store 6 (DirAddr 19)
  , Load (ImmValue 0) 6
  , Store 6 (DirAddr 20)
  , Load (DirAddr 17) 6
  , Push 6
  , Load (DirAddr 17) 6
  , Pop 5
  , Compute Mul 5 6 6
  , Push 6
  , Load (DirAddr 18) 6
  , Push 6
  , Load (DirAddr 18) 6
  , Pop 5
  , Compute Mul 5 6 6
  , Pop 5
  , Compute Add 5 6 6
  , Push 6
  , Load (ImmValue 4) 6
  , Push 6
  , Load (DirAddr 9) 6
  , Push 6
  , Load (ImmValue 2) 6
  , Pop 5
  , Compute Mul 5 6 6
  , Pop 5
  , Compute LShift 5 6 6
  , Pop 5
  , Compute Lt 5 6 6
  , Push 6
  , Load (DirAddr 19) 6
  , Push 6
  , Load (ImmValue 16) 6
  , Pop 5
  , Compute Lt 5 6 6
  , Pop 5
  , Compute And 5 6 6
  , Load (ImmValue 1) 5
  , Compute Xor 6 5 6
  , Branch 6 (Rel 50)
  , Load (DirAddr 17) 6
  , Store 6 (DirAddr 20)
  , Load (DirAddr 20) 6
  , Push 6
  , Load (DirAddr 20) 6
  , Pop 5
  , Compute Mul 5 6 6
  , Push 6
  , Load (DirAddr 18) 6
  , Push 6
  , Load (DirAddr 18) 6
  , Pop 5
  , Compute Mul 5 6 6
  , Pop 5
  , Compute Sub 5 6 6
  , Push 6
  , Load (DirAddr 9) 6
  , Pop 5
  , Compute RShift 5 6 6
  , Push 6
  , Load (DirAddr 15) 6
  , Pop 5
  , Compute Add 5 6 6
  , Store 6 (DirAddr 17)
  , Load (ImmValue 2) 6
  , Push 6
  , Load (DirAddr 20) 6
  , Pop 5
  , Compute Mul 5 6 6
  , Push 6
  , Load (DirAddr 18) 6
  , Pop 5
  , Compute Mul 5 6 6
  , Push 6
  , Load (DirAddr 9) 6
  , Pop 5
  , Compute RShift 5 6 6
  , Push 6
  , Load (DirAddr 14) 6
  , Pop 5
  , Compute Add 5 6 6
  , Store 6 (DirAddr 18)
  , Load (DirAddr 19) 6
  , Push 6
  , Load (ImmValue 1) 6
  , Pop 5
  , Compute Add 5 6 6
  , Store 6 (DirAddr 19)
  , Jump (Rel (-84))
  , Load (DirAddr 19) 6
  , Push 6
  , Load (ImmValue 16) 6
  , Pop 5
  , Compute Lt 5 6 6
  , Load (ImmValue 1) 5
  , Compute Xor 6 5 6
  , Branch 6 (Rel 15)
  , Load (ImmValue 1) 4
  , Load (DirAddr 3) 6
  , Push 6
  , Load (DirAddr 19) 6
  , Push 6
  , Load (ImmValue 2) 6
  , Pop 5
  , Compute RShift 5 6 6
  , Pop 5
  , Compute Add 5 4 5
  , Compute Add 5 6 6
  , Load (IndAddr 6) 6
  , WriteInstr 6 (DirAddr 65537)
  , Jump (Rel 22)
  , Load (DirAddr 19) 6
  , Push 6
  , Load (ImmValue 16) 6
  , Pop 5
  , Compute Lt 5 6 6
  , Load (ImmValue 1) 5
  , Compute Xor 6 5 6
  , Branch 6 (Rel 15)
  , Load (ImmValue 1) 4
  , Load (DirAddr 3) 6
  , Push 6
  , Load (DirAddr 19) 6
  , Push 6
  , Load (ImmValue 2) 6
  , Pop 5
  , Compute RShift 5 6 6
  , Pop 5
  , Compute Add 5 4 5
  , Compute Add 5 6 6
  , Load (IndAddr 6) 6
  , WriteInstr 6 (DirAddr 65537)
  , Jump (Rel 4)
  , Load (DirAddr 2) 6
  , WriteInstr 6 (DirAddr 65537)
  , Jump (Rel 1)
  , Jump (Rel (-154))
  , Load (DirAddr 1) 6
  , WriteInstr 6 (DirAddr 65537)
  , Jump (Rel (-177))
  , WriteInstr 0 (IndAddr 0)
  , ReadInstr (IndAddr 0)
  , Receive 6
  , EndProg
  ]
