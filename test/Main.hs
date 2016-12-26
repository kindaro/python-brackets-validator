module Main where

import Text.BracketsValidator

main = do
    test "(meow)" (Validation (State {position = 4, status = True}) [])
    test "(meow]" (Validation (State {position = 4, status = False}) [CSquare,ORound])

test expression expectation = do
        putStrLn $ "Testing " ++ (show expression) ++ " ..."
        putStrLn $ "Expectation: " ++ (show expectation)
        putStrLn $ "Result: " ++ (show result)
        case (result == expectation) of
            True -> putStrLn "Test passed."
            False -> putStrLn "Test failed."

    where result = (parser.lexer) expression
