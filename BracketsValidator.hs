module Test.BracketsValidator where

data Validation = Validation [Symbol] | Error | Ok deriving (Show)

data Symbol = ORound | OSquare | OCurled | CRound | CSquare | CCurled | Blank
    deriving (Eq, Show)

instance Read Symbol where
    readsPrec d s = [(readSymbol s, "")]
        where
        readSymbol s
            | s == "(" = ORound
            | s == "[" = OSquare
            | s == "{" = OCurled
            | s == ")" = CRound
            | s == "]" = CSquare
            | s == "}" = CCurled
            | otherwise = Blank

dropBlanks :: [Symbol] -> [Symbol]
dropBlanks = filter (/= Blank)

lexer :: String -> [Symbol]
lexer = fmap (read . (:[]))


validate = validator (Validation [])

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


