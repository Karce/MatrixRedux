# MatrixRedux
MatrixRedux is my implementation of matrix row reduction in Haskell.

MatrixRedux will reduce a matrix to reduced row echelon form.

To run this, compile MatrixRun.hs and MatrixFunctions.hs with ghc.
Then execute the binary "MatrixRun" with the desired matrix in the first argument.
Do not include spaces in the argument yet.

A valid matrix argument looks like this: [[4,0,3,5],[5,3,0,5],[0,0,3,6]]

An invalid argument looks like this: [[4, 0, 3, 5],[5, 3, 0, 5],[0, 0, 3, 6]]

Not all matrices are currently working. There must be a pivot in every diagonal position of the rows starting from the top left and incrementing to the bottom right.
