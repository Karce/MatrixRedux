module MatrixFunctions
( scaleRow
) where

scaleRow scalar row matrix = (take (row) matrix) ++ [(map (scalar*) (matrix !! row))] ++ (drop (row+1) matrix)
