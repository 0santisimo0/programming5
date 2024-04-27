{-# LANGUAGE UndecidableInstances #-}
module Scanner where
import Data.Char (isAlphaNum)


type Col = Int
type Line = Int
type Value = String
type Input = String

data Token = Token Type Value Line Col

data Type = String
          | OpenBlock
          | EndBlock
          | OpenTable
          | EndTable
          | OpenRow
          | EndRow
          | Keyword
          | EndSlide
          | Error
          | Comment
        deriving(Eq, Ord)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "
    show Comment = "Comment: "
    show OpenTable = "Table: "
    show EndTable = "EndTable: "
    show OpenRow = "Row: "
    show EndRow = "EndRow: "
    -- show ImageSlide = "Image: "

instance (Eq Type) => (Eq Token) where
    (Token String _ _ _) == (Token String _ _ _) = True
    (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
    (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
    (Token OpenTable _ _ _) == (Token OpenTable _ _ _) = True
    (Token EndTable _ _ _) == (Token EndTable _ _ _) = True
    (Token OpenRow _ _ _) == (Token OpenRow _ _ _) = True
    (Token EndRow _ _ _) == (Token EndRow _ _ _) = True
    (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
    (Token Error k1 _ _) == (Token Error k2 _ _) = k1 == k2
    (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
    (Token t1 s1 _ _ ) == (Token t2 s2 _ _ ) = t1 == t2 && s1 == s2


instance Ord Token where
    compare :: Token -> Token -> Ordering
    compare x y | x == y = EQ
                | x <= y = LT
                | otherwise = GT
    (Token t1 s1 _ _ ) <= (Token t2 s2 _ _ ) = t1 < t2 || (t1 == t2 && s1 <= s2)

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan ('#' : '#' : '#' : xs) l c = Token Keyword "###" l c : scan xs l (c + 3)
scan ('#' : '#' : xs) l c = Token Keyword "##" l c : scan xs l (c + 2)
scan ('<':'<':xs) l c = Token OpenRow "<<" l c : scan xs l (c + 1)
scan ('>':'>':xs) l c = Token EndRow ">>" l c : scan xs l (c + 1)
scan (x : xs) l c
  | x == '!' = Token Keyword [x] l c : scan xs l (c + 1)
  | x == '#' = Token Keyword [x] l c : scan xs l (c + 1)
  | x == '+' = Token Keyword [x] l c : scan xs l (c + 1)
  | x == '%' = getImageLink (x:xs) l c
  | x == ' ' = scan xs l (c + 1)
  | x == '\n' = scan xs (l + 1) 1
  | x == '{' = Token OpenBlock [x] l c : scan xs l (c + 1)
  | x == '}' = Token EndBlock [x] l c : scan xs l (c + 1)
  | x == '<' = Token OpenTable [x] l c : scan xs l (c + 1)
  | x == '>' = Token EndTable [x] l c : scan xs l (c + 1)
  | x == '*' && head xs == '*' && head (tail xs) == '*' = Token EndSlide "**" l c : scan (drop 2 xs) l (c + 2)
  | isAlphaNum x = let (word, rest) = span isAlphaNumOrSpace xs
                   in Token String (x:word) l c : scan rest l (c + length word + 1)
  | otherwise = Token Error [x] l c : scan xs l (c + 1)
  where isAlphaNumOrSpace = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', ':', ';', '-', '.', ',', '(', ')']))


getImageLink :: Input -> Line -> Col -> [Token]
getImageLink [] _ _ = []
getImageLink (x:xs) l c
    | x == '%' = Token Keyword [x] l c : getImageLink xs l (c + 1)
    | isAlphaNumOrSpace x =
        let (word, rest) = span isAlphaNumOrSpace xs
        in Token String (x:word) l c : scan rest l (c + length word + 1)
    | otherwise = [Token String "Error" l c]
  where
    isAlphaNumOrSpace = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [':', ';', '-', '.', ',', '/', '_']))
