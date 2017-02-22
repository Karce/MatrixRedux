-- Copyright 2017 Keaton Bruce
--
-- This file is part of MatrixRedux.
--
-- MatrixRedux is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- MatrixRedux is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with MatrixRedux. If not, see <http://www.gnu.org/licenses/>.
--

module MatrixFunctions
( scaleRow
) where

scaleRow :: Num a => a -> Int -> [[a]] -> [[a]]
scaleRow scalar row matrix = (take row matrix) ++ [map (scalar*) (matrix !! row)] ++ (drop (row+1) matrix)

replaceRow :: Num a => a -> Int -> Int -> [[a]] -> [[a]]
replaceRow scalar origRow replacedRow matrix = (take replacedRow matrix) ++ [subtractRows (map (scalar*) (matrix !! origRow)) (matrix !! replacedRow)] ++ (drop (replacedRow + 1) matrix)

subtractRows :: Num a => [a] -> [a] -> [a]
subtractRows xs ys = if not (null xs) && not (null ys) then ((head xs) - (head ys)) : (subtractRows (tail xs) (tail ys)) else []
