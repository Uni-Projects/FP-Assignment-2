-- Exercise 2.2 week 2
-- Johan Urban s1024726
-- Paolo Scattolin s1023775

module Char
where
import Data.Char

-- equal      :: String -> String -> Bool
-- isNumeral  :: String -> Bool
-- isBlank    :: String -> Bool
-- fromDigit  :: Char -> Int
-- toDigit    :: Int -> Char
-- shift      :: Int -> Char -> Char

msg  ::  String
msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
        \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

equal :: String -> String -> Bool
equal xs ys = map toLower xs == map toLower ys

isNumeral :: String -> Bool
isNumeral xs = and (map isDigit xs)

isBlank :: String -> Bool
isBlank xs = and (map isSpace xs)

fromDigit :: Char -> Int
fromDigit x 
   | isDigit x = ord x - 48   -- retrieve the ascii value and deduct const
   | otherwise = -1 -- input was not a digit

toDigit :: Int -> Char
toDigit x 
   | x < 10 && x >= 0  = chr ( x + 48 ) -- x + 48 will be the ascii value for the correct digit, in respect to x
   | otherwise = 'X' -- input was not an Integer, or not a small enough Integer > 0 

shift :: Int -> Char -> Char
shift n c 
   | isLower c && (ord c + n <= 122) && isLetter c =  chr (ord c + n)   --standard case for lower case letter
   | isLower c && isLetter c = chr (97 + ( ord c - 122 ) + (n-1)) -- special cases for lower case letters z -> a
   | isUpper c && (ord c + n <= 90) && isLetter c = chr (ord c + n) -- standard case: upper case letters
   | isLetter c = chr (65 + (ord c - 90) + (n-1)) -- special case: upper case letters
   | otherwise = c -- for all non-letter chars

-- 'map (\x -> shift 19 x) msg in ghci returns:
-- "FABER EST SUAE QUISQUE FORTUNAE APPIUS CLAUDIUS CAECUS DICTUM ARCNUM EST NEUTRON"
