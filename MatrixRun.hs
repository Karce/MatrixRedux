import MatrixFunctions

main = do
    rows <- getLine
    columns <- getLine
    matrix <- getLine
    let scaledMatrix = scaleRow 2 1 (read matrix)
    putStrLn (show scaledMatrix)
