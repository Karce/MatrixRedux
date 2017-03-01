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

import MatrixFunctions
import System.Environment

-- Run this binary with the matrix as the first argument and do not include
-- spaces in the matrix. A valid matrix would be [[4,0,3,5],[5,3,0,5],[0,0,3,6]]
-- but an invalid matrix argument would be [[4, 0, 3, 5],[5,3,0,5],[0,0,3,6]].
-- Only the first argument is currently read.
main = do
    args <- getArgs
    let matrix = args !! 0
    putStrLn (show (simplify (read matrix)))
