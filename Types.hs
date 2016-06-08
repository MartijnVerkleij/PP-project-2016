module Types where

data Alphabet   = Int
                | Bool
                
                | Var
                | Pid
                | Type
                
                | If
                | Else
                | While
                | Procedure
                
                | Global
                | Fork
                | Join
                
                -- punctuation
                | BraceO
                | BraceC
                | ParO
                | ParC
                | Op
                | Comma
                | Semi
                | Ass
