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

> {-# LANGUAGE ForeignFunctionInterface #-}
> module Main where

> import Data.Text (pack, unpack, splitOn)
> import Data.String.Utils
> import Control.Applicative
> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Trans.Maybe

> import DataStructures
> import Evaluation
> import SimLTL

> import Foreign.C.Types
> import Foreign.C.String

> foreign import ccall "setupConn"       c_setupConn       :: IO Bool
> foreign import ccall "getNumReps"      c_getNumReps      :: IO Int
> foreign import ccall "getNumAgents"    c_getNumAgents    :: IO Int
> foreign import ccall "getNumTicks"     c_getNumTicks     :: IO Int
> foreign import ccall "getFragmentSize" c_getFragmentSize :: IO Int
> foreign import ccall "getFormula"      c_getFormula      :: IO CString

SETUP FUNCTIONS

> getSystemState :: Tick -> Int -> SystemState
> getSystemState tick nAgents = 
>   (tick, agentStates) where agentStates = [(tick,id) | id <- [0..(nAgents-1)]]

> getSystemTrace :: Int -> Int -> SystemTrace
> getSystemTrace nTicks nAgents = [ getSystemState t nAgents | t <- [0..(nTicks-1)] ] 

MAIN FUNCTION

> main = do setup <- c_setupConn
>           nTicks <- c_getNumTicks
>           nAgents <- c_getNumAgents
>           fragmentSize <- c_getFragmentSize
>           nReps <- c_getNumReps
>           let trace = getSystemTrace nTicks nAgents
>           c_formula1 <- c_getFormula
>           c_formula2 <- peekCString c_formula1
>           let formula = read c_formula2
>           res <- eval (Prob formula fragmentSize) (take nReps $ repeat trace) evalStateS 
>           putStrLn $ "***************************\nRESULT: " ++ (show res)

