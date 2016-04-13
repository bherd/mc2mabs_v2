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

Predicate (energy should be greater than t)

> energyGT :: Int -> String -> Bool
> energyGT t s = let [_,state] = split ":" s in
>                  (read state) > t

> transform :: String -> [Int]
> transform s = map read $ split ":" s  

> type State = [Int]

Produce a formula

> getFormula :: FormulaS State
> getFormula = let fct = energyGT 100 in 
>                preprocS $ GloballyS $ PredS (\s -> True)

Formulae for example

> type StateA = [Int]
> type StateS = [StateA]
> type TraceS = [StateS]

> swarmEnergy :: StateS -> Int
> swarmEnergy s = sum [ as!!1 | as <- s ] 

> greaterThan :: (StateS -> Int) -> Int -> StateS -> Bool
> greaterThan pred v s = (pred s) > v

> swarmEnergyGT :: Int -> StateS -> Bool
> swarmEnergyGT v s = greaterThan swarmEnergy v s

> phi_1 :: StateS -> Bool
> phi_1 s = swarmEnergyGT 0 s

> getFormula1 :: FormulaS State
> getFormula1 = preprocS $ GloballyS $ PredS phi_1

Start verification

> verifyTrace :: SystemTrace -> IO Double
> verifyTrace trace = 
>   do let prop = Prob getFormula1 100  -- create property
>      res <- eval prop [trace] evalStateS -- 
>      return res

> verifyTraces :: [SystemTrace] -> IO Double
> verifyTraces traces = 
>   do results <- mapM verifyTrace traces 
>      let res = sum results / (fromIntegral $ length results)
>      return res

> readTrace :: String -> IO (SystemTrace)
> readTrace fn = 
>   do csvFile <- readCSV fn
>      let res = map (map transform) csvFile
>      return res

> readAndVerifyTraces :: [String] -> IO Double
> readAndVerifyTraces fns = do traces <- mapM readTrace fns
>                              verifyTraces traces 

CHECKS FOR IJAOSE PAPER

> check1 :: [SystemTrace] -> IO Double
> check1 ts = 
>   eval prop ts evalStateS
>     where prop = Prob formula 100
>             where formula = preprocS $ GloballyS $ PredS (swarmEnergyGT 0) 

> grabbing :: StateA -> Bool
> grabbing s = s!!0 == 0

> searching :: StateA -> Bool
> searching s = s!!0 == 1 

> check2 :: [SystemTrace] -> IO Double
> check2 ts = 
>   tp formula1 formula2 ts evalStateS 
>     where formula1 = preprocS $ ForAgent Nothing $ preprocA $ PredA searching
>           formula2 = preprocS $ ForAgent Nothing $ preprocA $ PredA grabbing 

MAIN FUNCTION

> main = do let cwd = "/home/bherd/workspaces/cpp/ijaose/release/"
>           let ids = [0::Int .. 10]
>           let fns = [cwd ++ "trace" ++ show id ++ ".txt" | id <- ids]
>           traces <- mapM readTrace fns
>           putStrLn $ "Reading done."  
>           res <- check1 traces  
>           putStrLn "Verification done."
>           putStrLn $ "Result: " ++ show res

