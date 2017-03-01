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
, replaceRow
, interchange
, simplify
) where

-- scaleRow will return a matrix with the indicated row scaled by the scalar.
scaleRow :: (Fractional a, Num a) => a -> Int -> [[a]] -> [[a]]
scaleRow scalar row matrix
    = (take row matrix)
   ++ [map (scalar*) (matrix !! row)]
   ++ (drop (row+1) matrix)

-- replaceRow will return a matrix with a row that has been replaced
-- by scaling the original row and then adding that to the replaced row.
replaceRow :: (Fractional a, Num a) => a -> Int -> Int -> [[a]] -> [[a]]
replaceRow scalar origRow replacedRow matrix
    = (take replacedRow matrix)
   ++ [subtractRows (map (scalar*) (matrix !! origRow)) (matrix !! replacedRow)]
   ++ (drop (replacedRow + 1) matrix)

-- subtractRows will return a row that is the difference of two other rows.
subtractRows :: (Fractional a, Num a) => [a] -> [a] -> [a]
subtractRows xs ys =
    if not (null xs) && not (null ys) 
    then ((head ys) - (head xs)) : (subtractRows (tail xs) (tail ys)) 
    else []

-- interchange will return a matrix with the two rows swapped.
interchange :: (Fractional a, Num a) => Int -> Int -> [[a]] -> [[a]]
interchange row1 row2 matrix =
    if row1 < row2
    then   (take row1 matrix)
        ++ [(matrix !! row2)]
        ++ (if (row2 - row1) > 1
           then take (row2 - row1 - 1) (drop (row1 + 1) matrix)
           else [])
        ++ [(matrix !! row1)]
        ++ (drop (row2 + 1) matrix)
    else if row1 > row2
    then   (take row2 matrix)
        ++ [(matrix !! row1)]
        ++ (if (row1 - row2) > 1
           then take (row1 - row2 - 1) (drop (row2 + 1) matrix)
           else [])
        ++ [(matrix !! row2)]
        ++ (drop (row1 + 1) matrix)
    else matrix

-- replaceRowsBelow replaces every row below with zeroes in the current column.
replaceRowsBelow :: (Fractional a, Num a) => Int -> Int -> Int -> [[a]] -> [[a]]
replaceRowsBelow currentRow row column matrix =
    if currentRow >= (length matrix)
    then matrix
    else replaceRowsBelow
        (currentRow + 1)
        row
        column
        (replaceRow
            ((matrix !! currentRow) !! column) 
            row 
            currentRow 
            matrix
        )

-- replaceRowsAbove replaces every row above with zeroes in the currect column.
replaceRowsAbove :: (Fractional a, Num a) => Int -> Int -> Int -> [[a]] -> [[a]]
replaceRowsAbove currentRow row column matrix = 
    if currentRow == 0
    then (replaceRow ((matrix !! currentRow) !! column) row currentRow matrix)
    else
        (if currentRow < 0 
        then matrix
        else (replaceRowsAbove
                (currentRow - 1)
                row
                column
                (replaceRow 
                    ((matrix !! currentRow) !! column) 
                    row 
                    currentRow 
                    matrix
                )
             )
        )

-- simplify will reduce the given matrix to reduced row echelon form.
-- As of now, it only works on matrices with a pivot in each diagonal position
-- starting with the top left corner.
-- TODO: Normalize negatives. Support interchanging pivot rows.
simplify :: (Fractional a, Num a) => [[a]] -> [[a]]
simplify matrix =
    if (length matrix) > (length (head matrix))
    then recurseReduct 0 (length (head matrix)) matrix
    else recurseReduct 0 (length matrix) matrix

-- recurseReduct is a recursive function that drives the replaceRow functions.
-- It Increments diagonally from the top left of the matrix to the bottom right.
recurseReduct :: (Fractional a, Num a) => Int -> Int -> [[a]] -> [[a]]
recurseReduct count least matrix = 
    if (count < least)
    then recurseReduct
        (count + 1)
        (least)
        (replaceRowsAbove
            (count - 1)
            count
            count
            (replaceRowsBelow
                (count + 1)
                count
                count
                (scaleRow
                    (1/((matrix !! count) !! count))
                    count
                    matrix
                )
            )
        )
    else matrix
