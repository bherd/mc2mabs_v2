/*
 * Copyright (c) 2016 Benjamin C. Herd.
 *
 * This file is part of MC2MABS.
 *
 * MC2MABS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * MC2MABS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with MC2MABS. If not, see <http://www.gnu.org/licenses/>.
 */

> module SimLTL 
> ( FormulaA(..)
> , FormulaS(..)
> , preprocA
> , preprocS
> , evalStateS -- TODO: only for debugging purposes; should be removed
> , tp
> , rtf
> , rsf
> ) 
> where

> import DataStructures
> import Evaluation
> import Control.Monad

> import Foreign hiding (unsafePerformIO)
> import Foreign.Marshal.Array
> import Foreign.C.Types
> import System.IO.Unsafe

Necessary for making FormulaA and FormulaS instances of Show

> instance Show (a -> b) where
>   show _ = "function"

Predicates core functions (for calling predicate functions in C code)

> foreign import ccall "_predA"    c_predA    :: CSize -> CSize -> CSize -> Bool
> foreign import ccall "_predP"    c_predP    :: CSize -> CSize -> CSize -> Ptr CSize -> IO Bool

> pred_a :: Int -> Int -> Int -> Bool
> pred_a tick predId agentId = c_predA (fromIntegral tick) (fromIntegral predId) (fromIntegral agentId)

> pred_pIO :: Int -> Int -> Int -> [CSize] -> IO Bool
> pred_pIO tick predId nAgents agentIds
>     = withArray agentIds  $ \cinput -> 
>       c_predP (fromIntegral tick) (fromIntegral predId) (fromIntegral nAgents) cinput 

> pred_p :: Int -> Int -> [Int] -> Bool
> pred_p tick predId agentIds = 
>   let c_agents = map fromIntegral agentIds in
>         unsafePerformIO (pred_pIO tick predId (length c_agents) c_agents)

Grammar for agent formulae

> data FormulaA a = AtomA Bool |
>                   NotA (FormulaA a) |
>                   PredA Int |
>                   AndA (FormulaA a) (FormulaA a) |
>                   OrA (FormulaA a) (FormulaA a) |
>                   NextA (FormulaA a) |
>                   UntilA (FormulaA a) (FormulaA a) |
>                   ReleaseA (FormulaA a) (FormulaA a) |
>                   FinallyA (FormulaA a) |
>                   GloballyA (FormulaA a)
>                   deriving (Read, Show)

Grammar for system formulae

> data FormulaS a = AtomS Bool |
>                   NotS (FormulaS a) |
>                   PredS Int |
>                   AndS (FormulaS a) (FormulaS a) |
>                   OrS (FormulaS a) (FormulaS a) |
>                   NextS (FormulaS a) |
>                   UntilS (FormulaS a) (FormulaS a) |
>                   ReleaseS (FormulaS a) (FormulaS a) |
>                   FinallyS (FormulaS a) |
>                   GloballyS (FormulaS a) |
>                   ForAgent (Maybe Int) (FormulaA a) |
>                   ForAgents (FormulaA a) (FormulaS a)   
>                   deriving (Read, Show)

Some preprocessing (exploit equivalences)

> preprocA :: (FormulaA a) -> (FormulaA a)
> preprocA (FinallyA f)  = UntilA (AtomA True) $ preprocA f
> preprocA (GloballyA f) = ReleaseA (AtomA False) $ preprocA f 
> preprocA f             = f

> preprocS :: (FormulaS a) -> (FormulaS a)
> preprocS (FinallyS f)  = UntilS (AtomS True) $ preprocS f
> preprocS (GloballyS f) = ReleaseS (AtomS False) $ preprocS f 
> preprocS f             = f

Conjunction of two instances of FormulaA [TODO: could be made more clever by exploiting logical tautologies]

> andFA :: (FormulaA a) -> (FormulaA a) -> (FormulaA a)
> andFA f1 f2 = AndA f1 f2

> andFS :: (FormulaS a) -> (FormulaS a) -> (FormulaS a)
> andFS f1 f2 = AndS f1 f2

[TODO: complete the negation functions]

> negFA :: (FormulaA a) -> (FormulaA a)
> negFA f = NotA f  

> negFS :: (FormulaS a) -> (FormulaS a)
> negFS f = NotS f  

Disjunction of two instances of FormulaA [TODO: could be made more clever by exploiting logical tautologies]

> orFA :: (FormulaA a) -> (FormulaA a) -> (FormulaA a)
> orFA f1 f2 = OrA f1 f2

> orFS :: (FormulaS a) -> (FormulaS a) -> (FormulaS a)
> orFS f1 f2 = OrS f1 f2

> instance Joinable (FormulaA a) where
>   (&&&) = andFA
>   (|||) = orFA
>   (¬)   = negFA

> instance Joinable (FormulaS a) where
>   (&&&) = andFS
>   (|||) = orFS
>   (¬)   = negFS

> instance Joinable Bool where
>   (&&&) = (Prelude.&&)
>   (|||) = (Prelude.||)
>   (¬)   = not

Evaluation of an agent formula on a state

> evalStateA :: (FormulaA a) -> AgentState -> Result (FormulaA a)
> evalStateA (AtomA b) s        = if b then Success else Failure
> evalStateA (NotA f) s         = (¬)(evalStateA f s)  
> evalStateA (PredA p) s        = let (tick,id) = s in
>                                   if pred_a tick p id then Success else Failure
> evalStateA (AndA f1 f2) s     = (evalStateA f1 s) &&& (evalStateA f2 s) 
> evalStateA (OrA f1 f2) s      = (evalStateA f1 s) ||| (evalStateA f2 s) 
> evalStateA (NextA f) s        = Strict f 
> evalStateA (UntilA f1 f2) s   = let io1 = evalStateA f1 s
>                                     io2 = evalStateA f2 s
>                                     fo  = Strict $ UntilA f1 f2 in
>                                       io2 ||| (io1 &&& fo)
> evalStateA (ReleaseA f1 f2) s = let io1 = evalStateA f1 s
>                                     io2 = evalStateA f2 s
>                                     fo  = Weak $ ReleaseA f1 f2 in
>                                       io2 &&& (io1 ||| fo)
> evalStateA (FinallyA f) s     = let io = evalStateA f s
>                                     fo = Strict $ FinallyA f in 
>                                       io ||| fo
> evalStateA (GloballyA f) s    = let io = evalStateA f s
>                                     fo = Weak $ GloballyA f in
>                                       io &&& fo

Check a given formula for a given agent

> forAgent :: SystemState -> Int -> FormulaA a -> Result (FormulaS a)
> forAgent state i f = do let (tick,s) = state
>                         ev <- evalStateA f (s!!i)
>                         return $ ForAgent (Just i) ev

Evaluation of a system formula on a state

> evalStateS :: (FormulaS a) -> SystemState -> IO (Result (FormulaS a))
> evalStateS (AtomS b) s        = if b then return Success else return Failure
> evalStateS (NotS f) s         = do res <- evalStateS f s
>                                    return $ (¬)res
> evalStateS (PredS p) s        = let (tick, agentStates) = s in
>                                   let agentIds = [ snd as | as <- agentStates ] in
>                                     if pred_p tick p agentIds then return Success else return Failure
> evalStateS (AndS f1 f2) s     = do r1 <- evalStateS f1 s
>                                    r2 <- evalStateS f2 s
>                                    return $ r1 &&& r2
> evalStateS (OrS f1 f2) s      = do r1 <- evalStateS f1 s
>                                    r2 <- evalStateS f2 s
>                                    return $ r1 ||| r2
> evalStateS (NextS f) s        = return $ Strict f 
> evalStateS (UntilS f1 f2) s   = do io1 <- evalStateS f1 s
>                                    io2 <- evalStateS f2 s
>                                    let fo  = Strict $ UntilS f1 f2
>                                    return $ io2 ||| (io1 &&& fo)
> evalStateS (ReleaseS f1 f2) s = do io1 <- evalStateS f1 s
>                                    io2 <- evalStateS f2 s
>                                    let fo  = Weak $ ReleaseS f1 f2
>                                    return $ io2 &&& (io1 ||| fo)
> evalStateS (FinallyS f) s     = do io <- evalStateS f s
>                                    let fo = Strict $ FinallyS f
>                                    return $ io ||| fo
> evalStateS (GloballyS f) s    = do io <- evalStateS f s
>                                    let fo = Weak $ GloballyS f
>                                    return $ io &&& fo
> evalStateS (ForAgent a f) s   = case a of 
>                                   Just i  -> return $ forAgent s i f
>                                   Nothing -> do i <- pickRand [0..(length (snd s))-1]
>                                                 evalStateS (ForAgent (Just i) f) s 
> evalStateS (ForAgents f1 f2) s = let (tick,state) = s in
>                                    let agentEvals = zip [0::Int ..] $ map (\astate -> evalStateA f1 astate) state in
>                                      let s' = [ state!!idx | (idx,res) <- agentEvals, success res ] in
>                                        evalStateS f2 (tick,s')

***************** ADVANCED PROPERTIES **************************

Relative state frequency

> rsf :: Joinable f => FormulaS f 
>                   -> [SystemTrace] 
>                   -> StateEvalFunc (FormulaS f)
>                   -> IO Double
> rsf f ts fct = eval prop ts fct where prop = Prob f 1 

Relative transition frequency

> rtf :: FormulaS f
>     -> FormulaS f
>     -> [SystemTrace] 
>     -> StateEvalFunc (FormulaS f)
>     -> IO Double
> rtf f1 f2 ts fct = eval prop ts fct 
>                      where prop = Prob (AndS f1 (NextS f2)) 2 

Transition probability

> tp :: FormulaS f
>    -> FormulaS f
>    -> [SystemTrace] 
>    -> StateEvalFunc (FormulaS f)
>    -> IO Double
> tp f1 f2 ts fct = do r1 <- rtf f1 f2 ts fct
>                      r2 <- eval (Prob f1 1) ts fct
>                      return $ r1 / r2

Pattern-based causation

> causesP :: FormulaS f
>        -> FormulaS f
>        -> [SystemTrace]
>        -> StateEvalFunc (FormulaS f)
>        -> IO Bool
> causesP _ _ [] _     = return True 
> causesP f1 f2 ts fct = do let fragSize = length $ head ts
>                           let prop1 = Prob (FinallyS f1) fragSize
>                           r1 <- eval prop1 ts fct
>                           let prop2 = Prob (GloballyS $ (OrS (NotS f1) (FinallyS f2))) fragSize
>                           r2 <- eval prop2 ts fct 
>                           let prop3 = Prob (FinallyS f2) fragSize
>                           r3 <- eval prop3 ts fct
>                           return $ (r1 > 0) &&& (r2 > r3)

Influence-based causation

> causesI :: FormulaS f
>         -> FormulaS f
>         -> [SystemTrace]
>         -> [SystemTrace]
>         -> StateEvalFunc (FormulaS f)
>         -> Double
>         -> IO Bool
> causesI _ _ [] _ _ _          = return True
> causesI _ _ _ [] _ _          = return True
> causesI f1 f2 ts1 ts2 fct thr = do let fragSize = length $ head ts1 
>                                    let prop1 = Prob (FinallyS f1) fragSize
>                                    res1 <- eval prop1 ts1 fct 
>                                    let prop2 = Prob (FinallyS f2) fragSize
>                                    res2 <- eval prop2 ts2 fct 
>                                    return $ (res2 - res1) > thr
>                                  
