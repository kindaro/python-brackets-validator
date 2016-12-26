module Text.BracketsValidator where

data Symbol = ORound | OSquare | OCurled | CRound | CSquare | CCurled | Blank String
    deriving (Eq, Show, Read)

isOpen x = case x of
    ORound -> True
    OSquare -> True
    OCurled -> True
    _ -> False

isClose x = case x of
    CRound -> True
    CSquare -> True
    CCurled -> True
    _ -> False

isBlank x = case x of
    Blank _ -> True
    _ -> False

isMatching ORound CRound = True
isMatching OSquare CSquare = True
isMatching OCurled CCurled = True
isMatching _ _ = False

lexer :: String -> [Symbol]
lexer [] = []
lexer (x:xs)
    | x == '(' = proceed ORound
    | x == '[' = proceed OSquare
    | x == '{' = proceed OCurled
    | x == '}' = proceed CCurled
    | x == ']' = proceed CSquare
    | x == ')' = proceed CRound
    | otherwise = case lexer xs of
        (Blank string) : _ -> Blank (x:string) : lexer (drop (length string) xs) -- Lookahead!
        _ -> proceed $ Blank (x:[])
    where proceed = (: lexer xs)

insert :: Symbol -> [Symbol] -> Validation [Symbol]
insert (Blank _) a = pure a
insert s a
    | isOpen s = pure (s:a)
    | (not . null) a && (head a) `isMatching` s = pure (tail a)
    | otherwise = impure (s:a)

data State = State { position :: Integer, status :: Bool }
    deriving (Eq, Read, Show)

data Validation x = Validation State x
    deriving (Eq, Read, Show)

instance Monoid State where
    mempty = State { position = 1, status = True }
    x `mappend` y
        | status x && status y = State { position = position x + position y, status = True }
        | otherwise = State { position = position x + position y, status = False }

instance Functor Validation where
    fmap f (Validation s a) = Validation s (f a)

instance Applicative Validation where
    pure x = Validation mempty x
    (Validation s f) <*> (Validation t x) = Validation (s `mappend` t) (f x)

taint (Validation s a) = Validation (s { status = False }) a
impure x = taint $ pure x

instance Monad Validation where
    (Validation s x) >>= f
        | status s == False = impure (unf x) -- Do nothing.
        | otherwise = s `into` f x
        -- Maybe I don't need this part.
        where unpack (Validation s a) = a
              unf = unpack . f
              into s (Validation t y) = Validation (s `mappend` t) y

parser :: [Symbol] -> Validation [Symbol]
parser = ( foldl (>>=) (return []) ) . (fmap insert)

report :: Validation [Symbol] -> String
report (Validation state stack)
    | status state && length stack == 0
        = "Validation succeeded with " ++ (show $ position state) ++ " symbols parsed."
    | status state
        = "Validation incomplete with " ++ (show $ position state) ++ " symbols parsed and "
            ++ (show $ length stack) ++ " symbols left in stack."
    | otherwise = "Validation failed after " ++ (show $ position state) ++ " symbols parsed. "
            ++ (show $ length stack) ++ " symbols left in stack."
