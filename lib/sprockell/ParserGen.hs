module ParserGen where

-- ===========================================================================
-- Parser Generator
-- ===========================================================================

import BasicFunctions
import ParseBasis
import Data.List

-- ==========================================================================================================
-- endSkip for dealing with end of input during parsing

endSkip nt = case nt of
                Opt  _          -> True
                Rep0 _          -> True
                Alt  nts mts    -> all endSkip nts || all endSkip mts
                Rep1 nts        -> all endSkip nts
                _               -> False

-- ==========================================================================================================
-- Parser Generator
-- ----------------
--      NOTE:
--      - Grammar gr is *function*
--      - nt is non-terminal; nt:rule is the rule under consideration
--      - nt0 is the father node
--      - ts is the list of subtrees under nt0 produced so far
--      - tokens here is the list of *indexed* input tokens
--      - recCheck is used for checking left-recursiveness of the grammar
-- ==========================================================================================================

parserGen :: Grammar -> [Alphabet] -> ParseState -> [(ParseTree,[(Int,Token)])]

parserGen gr []        (nt0,ts,tokens,recCheck)  = [(PNode nt0 ts, tokens)]

parserGen gr (nt:rule) (nt0,ts,[],recCheck)      | endSkip nt    = parserGen gr rule (nt0,ts,[],recCheck)
                                                 | otherwise     = [(PError (PNode nt0 ts) (nt:rule) nt "end of input" 0, [])]

parserGen gr (nt:rule) (nt0,ts, allTokens@((k,(cat,str)):remTokens), recCheck)

    | nt ∈ recCheck         = error ("grammar is left-recursive. Chain: " ++ show (recCheck ++ [nt]))
    | otherwise             = case nt of

        -- ============================================================================================================
        -- Backus-Naur constructions

        Alt nts mts     ->    parserGen gr (nts++rule)                (nt0,ts,allTokens,recCheck)
                           ++ parserGen gr (mts++rule)                (nt0,ts,allTokens,recCheck)

        Opt  nts        ->    parserGen gr (nts++rule)                (nt0,ts,allTokens,recCheck)
                           ++ parserGen gr  rule                      (nt0,ts,allTokens,recCheck)

        Rep0 nts        ->    parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,allTokens,recCheck)
                           ++ parserGen gr  rule                      (nt0,ts,allTokens,recCheck)

        Rep1 nts        ->    parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,allTokens,recCheck)

        -- ============================================================================================================
        -- Terminal Symbols

        TermSymb str'   | str==str'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        SyntCat cat'    | cat==cat'     -> parserGen gr rule (nt0, ts++[PLeaf (cat,str)], remTokens, [])
                        | otherwise     -> [(PError (PNode nt0 ts) (nt:rule) nt str k, [])]

        -- ============================================================================================================
        -- Non-terminals

        _  ->  concat [ nextParses
                        | r <- gr nt
                        , let parses        = parserGen gr r (nt,[],allTokens, recCheck++[nt])
                        , let correctParses = filter (not.isPError.fst) parses

                        , let nextParses | null correctParses = [ (finalPError (nt0,ts) $ maximum $ map fst parses , []) ]

                                         | otherwise          = concat $ map (parserGen gr rule) nextParseStates
                                                              where
                                                                nextParseStates = [ (nt0,ts++[t],remTokens,[])
                                                                                    | (t,remTokens) <- correctParses ]
                      ]

-- ==================================================
-- Additional functions

isPError (PError _ _ _ _ _) = True
isPError _                  = False

finalPError (nt0,ts) (PError t rule nt str k) = PError (PNode nt0 (ts++[t])) rule nt str k

-- ==================================================
-- Top-level parse function

parse :: Grammar -> Alphabet -> [Token] -> ParseTree

parse gr s tokens | null correctParses = maximum $ map fst parses
                  | not $ null rest    = error ("tokenList not fully parsed. Still left: " ++ (show $ map snd rest))
                  | otherwise          = final
          where
            parses = [ (t,rem) | r <- gr s
                               , (t,rem) <- parserGen gr r (s,[],tokens',[])
                               ]

            tokens' = zip [0..] tokens                        -- indexed tokens

            correctParses = filter (not.isPError.fst) parses

            (final,rest)  = head correctParses
