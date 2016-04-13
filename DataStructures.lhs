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

> module DataStructures where

> import Control.Monad
> import System.Random
> import Text.Parsec.Error
> import Text.Parsec.ByteString
> import Data.CSV
> import Data.List.Split
> import Data.String.Utils
> import System.IO.Strict
> import Foreign
> import Foreign.C.Types

******** DATA STRUCTURES ****************************

> type Tick    = Int
> type AgentId = Int

> type AgentState = (Tick, AgentId)
> type AgentTrace = [AgentState]

> type SystemState = (Tick, [AgentState])
> type SystemTrace = [SystemState]

******** DATA STRUCTURE OPERATIONS ******************

Convert a system trace to a set of agent traces

 strace2atraces :: SystemTrace -> [AgentTrace] 
 strace2atraces []    = []
 strace2atraces trace = [ [ state!!a | state <- trace ] | a <- [0..(length (head trace)-1)] ]

Convert a set of agent traces to a system trace

 atraces2strace :: [AgentTrace] -> SystemTrace
 atraces2strace []     = [] 
 atraces2strace traces = [ [ t!!i | t <- traces ] | i <- [0..(length (head traces)-1)] ]

Convert the content of a CSV file ([[String]]) to a system trace

 csv2strace :: [[String]] -> (String -> AgentState) -> SystemTrace
 csv2strace lines f = [ [ f astate | astate <- line] | line <- lines ]

Turn a trace into a list of fragments

> fragment :: [a] -> Int -> [[a]]
> fragment l k = [ take k $ drop i l | i <- [0..(length l)-k] ]

Randomly pick an element from a list

> pickRand :: [a] -> IO a
> pickRand l = do idx <- randomRIO (0, (length l - 1))
>                 return $ l!!idx

******** FILE OPERATIONS ******************

 readCSV_ :: String -> IO (Either ParseError [[String]])
 readCSV_ fn = parseFromFile csvFile fn

> readCSV :: String -> IO [[String]]
> readCSV fn = do content <- System.IO.Strict.readFile fn
>                 let lines = Data.String.Utils.split "\n" content
>                 let result = map (Data.String.Utils.split ",") lines
>                 return $ filter (\s -> length s > 0) result
