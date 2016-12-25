module Test.BracketsValidator where

data Validation = Validation [Symbol] | Error | Ok deriving (Show)

data Symbol = ORound | OSquare | OCurled | CRound | CSquare | CCurled | Blank String
    deriving (Eq, Show, Read)

lexer :: String -> [Symbol]
lexer [] = []
lexer (x:xs)
    | x == '(' = proceed ORound
    | x == '[' = proceed OSquare
    | x == '{' = proceed OCurled
    | x == ')' = proceed CRound
    | x == ']' = proceed CSquare
    | x == '}' = proceed CCurled
    | otherwise = case lexer xs of
        (Blank string) : _ -> Blank (x:string) : lexer (drop (length string) xs) -- Lookahead!
        _ -> proceed $ Blank (x:[])
    where proceed = (: lexer xs)
        
validate = show . validator (Validation []) . (lexer)

validator :: Validation -> [Symbol] -> Validation
validator Error _ = Error
validator (Validation []) [] = Ok
validator v [] = Error
validator (Validation []) (x:xs)
    | x == ORound || x == OSquare || x == OCurled
        = validator (Validation [x]) xs
    | x == CRound || x == CSquare || x == CCurled
        = Error
    | otherwise = validator (Validation []) xs
validator (Validation v@(w:ws)) (x:xs)
    | x == ORound || x == OSquare || x == OCurled
        = validator (Validation (x:v)) xs
    | (w == ORound && x == CRound) || (w == OSquare && x == CSquare) || (w == OCurled && x == CCurled)
        = validator (Validation ws) xs
    | otherwise = Error

main = interact validate


