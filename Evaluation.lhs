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

> module Evaluation 
> ( eval
> , evalTraceResult -- TODO: only for debugging purposes; should be removed
> , StateEvalFunc 
> , Joinable(..)
> , Property(..)
> , Result(..)
> , success
> ) where  

> import Prelude hiding ( (>>=), return )
> import Control.Monad
> import Control.Applicative
> import Debug.Trace
> import Data.Foldable

> import DataStructures

External functions to be called on the C side

> foreign import ccall "_setupConf"          c_setupConf          :: IO Bool
> foreign import ccall "_setupRun"           c_setupRun           :: IO Bool
> foreign import ccall "_tearDownConf"       c_tearDownConf       :: IO Bool
> foreign import ccall "_tearDownRun"        c_tearDownRun        :: IO Bool

We first need to define our own restricted Monad (for joinable types)

class JMonad m where
  return :: (Joinable a) => a -> m a
  (>>=)  :: (Joinable a, Joinable b) => m a -> (a -> m b) -> m b

Variant of foldM tailored to JMonad [TODO: check corRectness]

foldJM :: (JMonad m, Joinable a) => (a -> b -> m a) -> a -> [b] -> m a
foldJM fct a [b]    = fct a b
foldJM fct a (b:bs) = (foldJM fct a bs) >>= (\a' -> fct a b)

**************** RESULTS  **********************

> data Result a = Success | 
>                 Failure | 
>                 Strict a |
>                 Weak a |
>                 RAnd (Result a) (Result a) |
>                 ROr (Result a) (Result a)
>                 deriving (Eq,Show) 

> instance Functor Result where
>   fmap f Success = Success
>   fmap f Failure = Failure
>   fmap f (Strict a) = Strict $ f a
>   fmap f (Weak a) = Weak $ f a
>   fmap f (RAnd r1 r2) = RAnd (fmap f r1) (fmap f r2)
>   fmap f (ROr r1 r2)  = ROr (fmap f r1) (fmap f r2)

> bindR :: Result a -> (a -> Result b) -> Result b
> bindR x k = case x of 
>              Success    -> Success
>              Failure    -> Failure
>              Strict o   -> k o
>              Weak o     -> k o
>              RAnd r1 r2 -> RAnd (r1 >>= k) (r2 >>= k)
>              ROr r1 r2  -> ROr (r1 >>= k) (r2 >>= k)

> instance Monad Result where
>   return = Strict
>   (>>=)  = bindR

instance JMonad Result where
 return = Strict
 (>>=)  = bindR

> class Joinable a where
>   (&&&)   :: a -> a -> a
>   (|||)   :: a -> a -> a
>   (¬)     :: a -> a

> instance Joinable a => Joinable (Result a) where
>   (&&&)   = andR
>   (|||)   = orR
>   (¬)   = negR

Convenience definition

> type StateEvalFunc b = (b -> SystemState -> IO (Result b))

Conjunction of results  

> andR :: Joinable a => Result a -> Result a -> Result a
> andR Failure _                 = Failure  -- a conjunction with Failure is always a failure
> andR _ Failure                 = Failure  
> andR Success r                 = r        -- conjoining Success with r is always r
> andR r Success                 = r
> andR (Strict o1) (Strict o2)   = Strict $ o1 &&& o2
> andR (Strict o1) (Weak o2)     = RAnd (Strict o1) (Weak o2) 
> andR (Strict o) (RAnd r1 r2)   = andR (Strict o) (andR r1 r2)
> andR (Strict o) (ROr r1 r2)    = andR (Strict o) (orR r1 r2)
> andR (Weak o1) (Strict o2)     = RAnd (Weak o1) (Strict o2)
> andR (Weak o1) (Weak o2)       = Weak $ o1 &&& o2 
> andR (Weak o) (RAnd r1 r2)     = andR (Weak o) (andR r1 r2)
> andR (Weak o) (ROr r1 r2)      = andR (Weak o) (orR r1 r2)
> andR (RAnd r1 r2) (Strict o)   = andR (andR r1 r2) (Strict o)
> andR (RAnd r1 r2) (Weak o)     = andR (andR r1 r2) (Weak o) 
> andR (RAnd r1 r2) (RAnd r3 r4) = andR (andR r1 r2) (andR r3 r4)
> andR (RAnd r1 r2) (ROr r3 r4)  = andR (andR r1 r2) (orR r3 r4)
> andR (ROr r1 r2) (Strict o)    = andR (orR r1 r2) (Strict o)
> andR (ROr r1 r2) (Weak o)      = andR (orR r1 r2) (Weak o) 
> andR (ROr r1 r2) (RAnd r3 r4)  = andR (orR r1 r2) (andR r3 r4)
> andR (ROr r1 r2) (ROr r3 r4)   = andR (orR r1 r2) (orR r3 r4)

Conjunction of results in the final state of a trace

> andR_final :: Joinable a => Result a -> Result a -> Result a
> andR_final Failure _                 = Failure
> andR_final _ Failure                 = Failure
> andR_final (Strict _) _              = Failure
> andR_final _ (Strict _)              = Failure
> andR_final Success Success           = Success
> andR_final Success (Weak o)          = Success
> andR_final Success (RAnd r1 r2)      = andR_final r1 r2
> andR_final Success (ROr r1 r2)       = orR_final r1 r2
> andR_final (Weak o) Success          = Success
> andR_final (Weak o1) (Weak o2)       = Success 
> andR_final (Weak o) (RAnd r1 r2)     = andR_final r1 r2
> andR_final (Weak o) (ROr r1 r2)      = orR_final r1 r2
> andR_final (RAnd r1 r2) Success      = andR_final r1 r2
> andR_final (RAnd r1 r2) (Weak o2)    = andR_final r1 r2 
> andR_final (RAnd r1 r2) (RAnd r3 r4) = andR_final (andR_final r1 r2) (andR_final r3 r4)
> andR_final (RAnd r1 r2) (ROr r3 r4)  = andR_final (andR_final r1 r2) (orR_final r3 r4)
> andR_final (ROr r1 r2) Success       = orR_final r1 r2
> andR_final (ROr r1 r2) (Weak o2)     = orR_final r1 r2  
> andR_final (ROr r1 r2) (RAnd r3 r4)  = andR_final (orR_final r1 r2) (andR_final r3 r4)
> andR_final (ROr r1 r2) (ROr r3 r4)   = andR_final (orR_final r1 r2) (orR_final r3 r4)

Disjunction of results

> orR :: Joinable a => Result a -> Result a -> Result a
> orR Success _                 = Success  -- a disjunction with Success is always Success
> orR _ Success                 = Success  
> orR Failure r                 = r        -- disjoining Failure with r is always r
> orR r Failure                 = r
> orR (Strict o1) (Strict o2)   = Strict $ o1 &&& o2
> orR (Strict o1) (Weak o2)     = ROr (Strict o1) (Weak o2) 
> orR (Strict o) (RAnd r1 r2)   = orR (Strict o) (andR r1 r2)
> orR (Strict o) (ROr r1 r2)    = orR (Strict o) (orR r1 r2)
> orR (Weak o1) (Strict o2)     = ROr (Weak o1) (Strict o2)
> orR (Weak o1) (Weak o2)       = Weak $ o1 &&& o2 
> orR (Weak o) (RAnd r1 r2)     = orR (Weak o) (andR r1 r2)
> orR (Weak o) (ROr r1 r2)      = orR (Weak o) (orR r1 r2)
> orR (RAnd r1 r2) (Strict o)   = orR (andR r1 r2) (Strict o)
> orR (RAnd r1 r2) (Weak o)     = orR (andR r1 r2) (Weak o) 
> orR (RAnd r1 r2) (RAnd r3 r4) = orR (andR r1 r2) (andR r3 r4)
> orR (RAnd r1 r2) (ROr r3 r4)  = orR (andR r1 r2) (orR r3 r4)
> orR (ROr r1 r2) (Strict o)    = orR (orR r1 r2) (Strict o)
> orR (ROr r1 r2) (Weak o)      = orR (orR r1 r2) (Weak o) 
> orR (ROr r1 r2) (RAnd r3 r4)  = orR (orR r1 r2) (andR r3 r4)
> orR (ROr r1 r2) (ROr r3 r4)   = orR (orR r1 r2) (orR r3 r4)

Disjunction of results in the final state of a trace

> orR_final :: Joinable a => Result a -> Result a -> Result a
> orR_final Success _                 = Success
> orR_final _ Success                 = Success
> orR_final (Weak _) _                = Success
> orR_final _ (Weak _)                = Success
> orR_final Failure Failure           = Failure
> orR_final Failure (Strict o)        = Failure
> orR_final Failure (RAnd r1 r2)      = andR_final r1 r2
> orR_final Failure (ROr r1 r2)       = orR_final r1 r2
> orR_final (Strict o) Failure        = Failure 
> orR_final (Strict o1) (Strict o2)   = Failure
> orR_final (Strict o) (RAnd r1 r2)   = andR_final r1 r2 
> orR_final (Strict o) (ROr r1 r2)    = orR_final r1 r2
> orR_final (RAnd r1 r2) Failure      = andR_final r1 r2
> orR_final (RAnd r1 r2) (Strict o)   = andR_final r1 r2 
> orR_final (RAnd r1 r2) (RAnd r3 r4) = orR_final (andR_final r1 r2) (andR_final r3 r4)
> orR_final (RAnd r1 r2) (ROr r3 r4)  = orR_final (andR_final r1 r2) (orR_final r3 r4)
> orR_final (ROr r1 r2) Failure       = orR_final r1 r2
> orR_final (ROr r1 r2) (Strict o)    = orR_final r1 r2 
> orR_final (ROr r1 r2) (RAnd r3 r4)  = orR_final (orR_final r1 r2) (andR_final r3 r4)
> orR_final (ROr r1 r2) (ROr r3 r4)   = orR_final (orR_final r1 r2) (orR_final r3 r4)

Negation of results

> negR :: Joinable a => Result a -> Result a
> negR Success      = Failure
> negR Failure      = Success
> negR (Weak o)     = Weak $ (¬) o 
> negR (Strict o)   = Strict $ (¬) o 
> negR (RAnd r1 r2) = negR $ andR r1 r2
> negR (ROr r1 r2)  = negR $ orR r1 r2

Negation of results in the final state of a trace [TODO: necessary?]

> negR_final :: Joinable a => Result a -> Result a
> negR_final Success      = Failure
> negR_final Failure      = Success
> negR_final (Weak o)     = Failure  
> negR_final (Strict o)   = Success  
> negR_final (RAnd r1 r2) = negR_final $ andR_final r1 r2
> negR_final (ROr r1 r2)  = negR_final $ orR_final r1 r2

Evaluation of a result on a system state

> evalStateResult :: (Joinable f) => SystemState      -- system state
>                                 -> Result f           -- result to be evaluated
>                                 -> StateEvalFunc f  -- state evaluation function
>                                 -> IO (Result f)      -- result
> evalStateResult s Success fct    = return Success
> evalStateResult s Failure fct    = return Failure
> evalStateResult s (Strict f) fct = fct f s
> evalStateResult s (Weak f) fct   = fct f s
> evalStateResult s (RAnd r1 r2) fct = do res1 <- evalStateResult s r1 fct
>                                         res2 <- evalStateResult s r2 fct
>                                         return $ res1 &&& res2
> evalStateResult s (ROr r1 r2) fct = do res1 <- evalStateResult s r1 fct
>                                        res2 <- evalStateResult s r2 fct
>                                        return $ res1 ||| res2

Evaluation of a result on a system trace  

> evalTraceResult :: (Joinable f) => SystemTrace      -- system trace
>                                 -> Result f           -- result to be evaluated
>                                 -> StateEvalFunc f  -- state evaluation function
>                                 -> IO (Result f)      -- result
> evalTraceResult [] _ _       = return Success
> evalTraceResult [a] r fct    = do res <- evalStateResult a r fct
>                                   case res of 
>                                     Success    -> return Success
>                                     Failure    -> return Failure 
>                                     Weak _     -> return Success
>                                     Strict _   -> return Failure
>                                     RAnd r1 r2 -> return $ andR_final r1 r2
>                                     ROr r1 r2  -> return $ orR_final r1 r2
> evalTraceResult (x:xs) r fct = do obl <- evalStateResult x r fct
>                                   evalTraceResult xs obl fct

> evalTraceResult2 :: (Joinable f) => SystemTrace     -- system trace
>                                 -> Result f           -- result to be evaluated
>                                 -> StateEvalFunc f  -- state evaluation function
>                                 -> IO (Result f)      -- result
> evalTraceResult2 t r fct = do res <- foldM (\r s -> evalStateResult s r fct) r t 
>                               truncateResult res

> evalTraceResult3 :: (Show f, Joinable f) => SystemTrace      -- system trace
>                                          -> Result f           -- result to be evaluated
>                                          -> StateEvalFunc f  -- state evaluation function
>                                          -> IO (Result f)      -- result
> evalTraceResult3 t r fct = do putStrLn "Starting new trace" 
>                               r1 <- evalStateResult (t!!0) r fct
>                               putStrLn $ "State 1: " ++ show r1
>                               r2 <- evalStateResult (t!!1) r1 fct
>                               putStrLn $ "State 2: " ++ show r2
>                               r3 <- evalStateResult (t!!2) r2 fct
>                               putStrLn $ "State 3: " ++ show r3
>                               res <- truncateResult r3
>                               putStrLn $ "Result: " ++ show res
>                               return res

Produce the final result at the end of the trace

> truncateResult :: Joinable f => Result f -> IO (Result f)
> truncateResult Success      = return Success
> truncateResult Failure      = return Failure
> truncateResult (Strict _)   = return Failure
> truncateResult (Weak _)     = return Success
> truncateResult (RAnd r1 r2) = return $ r1 `andR_final` r2
> truncateResult (ROr r1 r2)  = return $ r1 `orR_final` r2

Evaluation of a result on a set of system traces

> evalTracesResult :: (Show f, Joinable f) => [SystemTrace] -- list of system traces
>                             -> Result f                     -- formula
>                             -> Int                          -- fragment size
>                             -> StateEvalFunc f            -- state evaluation function
>                             -> IO [Result f]                -- result
> evalTracesResult ts r k fct = 
>   do let fragments = Prelude.concat $ map (\t -> fragment t k) ts -- turn trace into fragments
>      results <- forM fragments (\t -> do putStr "Running simulation ... "
>                                          setup_res <- c_setupRun
>                                          res <- evalTraceResult t r fct
>                                          putStrLn $ "done (result = " ++ (show res) ++ ")."
>                                          tearDown_res <- c_tearDownRun
>                                          return res)   -- call trace evaluation function for each trace in list
>      return results

> evalTracesResult2 :: (Show f, Joinable f) => [SystemTrace] -- list of system traces
>                             -> Result f                     -- formula
>                             -> Int                          -- fragment size
>                             -> StateEvalFunc f            -- state evaluation function
>                             -> IO [Result f]                -- result
> evalTracesResult2 ts r k fct = 
>   do let fragmentedTraces = map (\t -> fragment t k) ts -- turn trace into fragments
>      results <- forM fragmentedTraces (\t -> do putStr "Running simulation ... "
>                                                 setup_res <- c_setupRun
>                                                 results <- forM t (\frag -> evalTraceResult frag r fct)  
>                                                 putStrLn $ "done (result = " ++ (show results) ++ ")."
>                                                 tearDown_res <- c_tearDownRun
>                                                 return results)   -- call trace evaluation function for each trace in list
>      return $ Prelude.concat results

Helper function to check whether a formula has already been satisfied 
(in order to avoid having to make 'Result a' an instance of Eq)

> success :: Result a -> Bool
> success Success = True
> success _       = False

**************** PROPERTIES AND THEIR EVALUATION *************************

> data Property a = Prob a Int
>                   deriving (Show) 

Evaluate a property; determine the probability of a property being true 
(= ratio of successful and unsuccessful checks)

> eval :: (Show f, Joinable f) => Property f -- property to be checked
>                      -> [SystemTrace]    -- list of system traces
>                      -> StateEvalFunc f  -- state evaluation function 
>                      -> IO Double          -- result
> eval (Prob f k) ts fct = 
>   do c_setupConf
>      results <- evalTracesResult2 ts (Strict f) k fct  -- call traces evaluation function
>      let successes = filter (success) results         -- filter out failures
>      let nSucc = fromIntegral $ length successes      -- calculate number of successes
>      let nEval = fromIntegral $ length results        -- overall number of evaluations
>      c_tearDownConf
>      return $ nSucc / nEval                           -- ratio of successes and failures

*************** ADVANCED PROPERTIES *************************************

General correlation function

> corr :: (Show f, Joinable f) => Property f         -- property 1 
>                      -> Property f                 -- property 2
>                      -> (Double -> Double -> Bool) -- comparison function
>                      -> [SystemTrace]            -- list of system traces
>                      -> StateEvalFunc f          -- state evaluation function
>                      -> IO Bool                    -- result
> corr (Prob f1 k1) (Prob f2 k2) compFct ts fct = 
>   do pAndQ <- eval (Prob (f1 &&& f2) k1) ts fct    -- joint probability of P and Q
>      p <- eval (Prob f1 k1) ts fct                 -- probability of just P
>      q <- eval (Prob f2 k2) ts fct                 -- probability of just Q
>      return $ pAndQ `compFct` (p * q)              -- Pr(P /\ Q) [>,<,==] Pr(P)*Pr(Q)

Positive correlation

> posCorr :: (Show f, Joinable f) => Property f -- property 1 
>                         -> Property f         -- property 2
>                         -> [SystemTrace]    -- list of system traces
>                         -> StateEvalFunc f  -- state evaluation function
>                         -> IO Bool            -- result
> posCorr p1 p2 ts fct = corr p1 p2 (>) ts fct

Negative correlation

> negCorr :: (Show f, Joinable f) => Property f -- property 1
>                         -> Property f         -- property 2
>                         -> [SystemTrace]    -- list of system traces
>                         -> StateEvalFunc f  -- state evaluation function
>                         -> IO Bool            -- result
> negCorr p1 p2 ts fct = corr p1 p2 (<) ts fct

No correlation

> noCorr :: (Show f, Joinable f) => Property f  -- property 1
>                         -> Property f         -- property 2
>                         -> [SystemTrace]    -- list of system traces
>                         -> StateEvalFunc f  -- state evaluation function
>                         -> IO Bool            -- result
> noCorr p1 p2 ts fct = corr p1 p2 (==) ts fct
