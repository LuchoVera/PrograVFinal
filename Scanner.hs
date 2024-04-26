{-# LANGUAGE UndecidableInstances #-}
module Scanner where 
import Data.Char (isLetter,isDigit, isAlpha, isAlphaNum, isSpace, isSeparator, isPunctuation)
import Abstract (H2, Background (Background))

-- cabal install uulib

type Col = Int
type Line = Int 
type Value = String
type Input = String

data Token = Token Type Value Line Col

data Type = String 
          | OpenBlock
          | EndBlock
          | Keyword
          | Error
          | H1
          | H2
          | H3
          | Img
          | Bold
          | Italik
          -- | Comment
          | EndSlide
          | EndLine
          | ElemList
    
        deriving(Eq,Ord)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "
    show H1 = "H1: "
    show H2 = "H2: "
    show H3 = "H3: "
    show Bold = "Bold: "
    show Italik = "Italik: "
    show Img = "Img: "
    show EndLine = "EndLine: "
    show ElemList = "List: "
    

instance (Eq Type) => (Eq Token) where
    (Token String _ _ _) ==(Token String _ _ _) = True
    (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
    (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
    (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
    (Token Error k1 _ _) == (Token Error k2 _ _) = k1 == k2
    (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
    (Token t1 s1 _ _ ) == (Token t2 s2 _ _ ) = t1 == t2 && s1 == s2

    
instance Ord Token where
    compare x y | x == y = EQ
                | x <= y = LT
                | otherwise = GT
    (Token t1 s1 _ _ ) <= (Token t2 s2 _ _ ) = t1 < t2 || (t1 == t2 && s1 <= s2)


scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c 
    | x == '!' = let (token, rest) = span (== '!') (x:xs)
                  in Token Keyword token l c : scan rest l (c + length token)
    | x == '^' = let (token, rest) = span (== '^') (x:xs)
                  in Token Keyword token l c : scan rest l (c + length token)
    | x == '$' = let (token, rest) = span (== '$') (x:xs)
                  in Token ElemList token l c : scan rest l (c + length token)
    | x == '/' = Token EndLine [x] l c : scan xs l (c + 1)
    
    | x == '~' = let (token, rest) = span (== '~') (x:xs)
                  in Token Img token l c : scan rest l (c + length token)
    | x == '#' && xs /= [] && head xs /= '#' && tail xs /= [] && head (tail xs) /= '#' = let (token, rest) = span (== '#') (x:xs)
                                                                                              in Token H1 token l c : scan rest l (c + length token)
    | x == '#' && xs /= [] && head xs == '#' && tail xs /= [] && head (tail xs) /= '#' = let (token, rest) = span (== '#') (x:xs)
                                                                                              in Token H2 token l c : scan rest l (c + length token)
    | x == '#' && xs /= [] && head xs == '#' && tail xs /= [] && head (tail xs) == '#' = let (token, rest) = span (== '#') (x:xs)
                                                                                              in Token H3 token l c : scan rest l (c + length token)
    | x == '#' && xs /= [] && head xs /= '#' && tail xs /= [] && head (tail xs) /= '#' = let (token, rest) = span (== '#') (x:xs)
                                                                                              in Token Italik token l c : scan rest l (c + length token)
    | x == '*' && xs /= [] && head xs == '*' && tail xs /= [] && head (tail xs) /= '*' = let (token, rest) = span (== '*') (x:xs)
                                                                                              in Token Bold token l c : scan rest l (c + length token)
    | x == '*' && xs /= [] && head xs /= '*' && tail xs /= [] && head (tail xs) /= '*' = let (token, rest) = span (== '*') (x:xs)
                                                                                              in Token Italik token l c : scan rest l (c + length token)
    | x == ' ' = scan xs l (c+1)
    | x == '\n' = scan xs (l + 1) 1
    
    | x == '{' = let (token, rest) = span (== '{') (x:xs)
                  in Token OpenBlock token l c : scan rest l (c + length token)
    
    | x == '}' = let (token, rest) = span (== '}') (x:xs)
                  in Token EndBlock token l c : scan rest l (c + length token)
    | x == '-' && xs /= [] && head xs == '-' && tail xs /= [] && head (tail xs) == '-' = let (token, rest) = span (== '-') (x:xs)
                                                                                              in Token EndSlide token l c : scan rest l (c + length token)
    -- | isLetter x || isDigit x  = let (token, rest) = span (\c -> isAlphaNum c || c == ' ' || isPunctuation c) (x:xs)
    --                             in Token String token l c : scan rest l (c + length token)
    | isValidChar x  = let (token, rest) = span isValidChar (x:xs)
                       in Token String token l c : scan rest l (c + length token)
    
    
    | otherwise = Token Error [x] l c : scan xs l (c + 1)


isValidChar :: Char -> Bool
isValidChar c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 :/.-,!=()%ñÑáéíóúÁÉÍÓÚ¿?"
