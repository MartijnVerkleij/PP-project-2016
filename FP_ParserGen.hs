module FP_ParserGen where

{- ===========================================================================
Contains Parser Generator
=========================================================================== -}


import Types
import Data.List

-- ==========================================================================================================
-- endSkip for dealing withend of input

endSkip nt = case nt of
                Opt  _          -> True
                Rep0 _          -> True
                Alt  nts mts    -> all endSkip nts || all endSkip mts
                Rep1 nts        -> all endSkip nts
                _               -> False

-- ==========================================================================================================
-- Parser Generator

parserGen :: Grammar -> [Alphabet] -> ParseState -> [(ParseTree,[Token])]

parserGen gr []        (nt0,ts,tokens)  = [(PNode nt0 ts, tokens)]

parserGen gr (nt:rule) (nt0,ts,[])      | endSkip nt    = parserGen gr rule (nt0,ts,[])
                                        | otherwise     = [(PError (PNode nt0 ts) (nt:rule) nt "end of input" 0, [])]

parserGen gr (nt:rule) (nt0,ts,(cat,str,k):tokens) = case nt of

        -- ============================================================================================================
        -- Backus-Naur constructions

        Alt nts mts     ->    parserGen gr (nts++rule) (nt0,ts,(cat,str,k):tokens)
                           ++ parserGen gr (mts++rule) (nt0,ts,(cat,str,k):tokens)

        Opt  nts        ->    parserGen gr (nts++rule) (nt0,ts,(cat,str,k):tokens)
                           ++ parserGen gr  rule       (nt0,ts,(cat,str,k):tokens)

        Rep0 nts        ->    parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str,k):tokens)
                           ++ parserGen gr  rule                      (nt0,ts,(cat,str,k):tokens)

        Rep1 nts        -> parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str,k):tokens)

        -- ============================================================================================================
        -- Terminals

        Terminal str'   | str==str'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str,k)], tokens)
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        Symbol str'     | str==str'     -> parserGen gr rule (nt0,ts,tokens)
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        SyntCat cat'    | cat==cat'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str,k)], tokens)
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        -- ============================================================================================================
        -- Non-terminals

        _  ->  concat [ nextParses | r <- gr nt
                                   , let parses        = parserGen gr r (nt,[],(cat,str,k):tokens)
                                   , let correctParses = filter (not.isPError.fst) parses

                                   , let nextParses | null correctParses = [ (finalPError (nt0,ts) $ maximum $ map fst parses , []) ]
                                                    | otherwise          = concat $ map (continueParsing gr rule (nt0,ts)) correctParses
                                   ]

-- ==================================================
-- Additional functions

isPError (PError _ _ _ _ _) = True
isPError _                  = False

continueParsing gr rule (nt0,ts) (t,tokens) = parserGen gr rule (nt0,ts++[t],tokens)

finalPError (nt0,ts) (PError t rule nt str k) = PError (PNode nt0 (ts++[t])) rule nt str k

-- ==================================================
parse :: Grammar -> Alphabet -> [Token] -> ParseTree -- [(ParseTree, [Token])]

parse gr s tokens | null correctParses = maximum $ map fst parses
                  | not $ null rest    = error "tokenList not fully parsed"
                  | otherwise          = final
          where
            parses = [ (t,rem) | r <- gr s
                               , (t,rem) <- parserGen gr r (s,[],tokens)
                               ]

            correctParses = filter (not.isPError.fst) parses

            (final,rest)  = head correctParses
