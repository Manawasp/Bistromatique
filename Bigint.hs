module Bigint (Bigint(Bigint), bigint_of_string, string_of_bigint, add) where

import Data.Char
--  __DATA__ --

--- infinite number ---
data Base = Binary | Octal | Decimal | Hexadecimal
data Sign = Positive | Negative
data Bigint = Bigint String Base Sign

--  !__DATA__ --

--- view Data  --
-- note : see deriving
--
showSign :: Sign -> String
showSign Positive = "Positive"
showSign Negative = "Negative"
instance Show Sign where
  show = showSign

showBase :: Base -> String
showBase Hexadecimal = "Hexadecimal"
showBase Binary = "Binary"
showBase Octal = "Octal"
showBase Decimal = "Decimal"
instance Show Base where
  show = showBase

showBigint :: Bigint -> String
showBigint (Bigint num base sign) = "(" ++ (show num) ++ " : " ++ (show base) ++ " : " ++ (show sign) ++ ")"
instance Show Bigint where
  show = showBigint
---

--- string to bigint ---
--
get_sign :: String -> Sign
get_sign ('-':_) = Negative
get_sign _ = Positive

get_base :: String -> Base
get_base ('-':q) = get_base q
get_base ('0':'x':_) = Hexadecimal
get_base ('0':'b':_) = Binary
get_base ('0':_) = Octal
get_base _ = Decimal

get_num :: String -> String
get_num ('-':q) = get_num q
get_num ('x':q) = get_num q
get_num ('b':q) = get_num q
get_num ('0':q) = get_num q
get_num str = str

bigint_of_string :: String -> Bigint
bigint_of_string str = Bigint (get_num str) (get_base str) (get_sign str)
--

--- bigint to string ---
--
get_sign' :: Sign -> String
get_sign' Positive = ""
get_sign' Negative = "-"

get_base' :: Base -> String
get_base' Hexadecimal = "0x"
get_base' Binary = "0b"
get_base' Octal = "0"
get_base' Decimal = ""

string_of_bigint :: Bigint -> String
string_of_bigint (Bigint numb base sign) = (get_sign' sign) ++ (get_base' base) ++ numb
--

--- Convert Char to Int ---
--
ord' :: Char -> Int
ord' c = ord(c) - ord('0')
--

--- Convert Int to Char ---
--
chr' :: Int -> Char
chr' c = chr(c + 48)
--

--- Add infinite number ---
--
add' :: String -> String -> Int -> String -> String
add' [] [] 0 new = new
add' [] [] ret new = add' [] [] 0 ((chr' ret):new)
add' [] (a':q') ret new = add' [] q' (((ord' a') + ret) `quot` 10) ((chr' (((ord' a') + ret) `mod` 10)):new)
add' (a':q') [] ret new = add' [] (a':q') ret new
add' (a:q) (a':q') ret new = add' q q' (((ord' a') + (ord' a) + ret) `quot` 10) ((chr' (((ord' a') + (ord' a) + ret) `mod` 10)):new)
--

--- Sub infinite number ---
ret' :: Int -> Int -> Int
ret' x y = if x >= y then 0 else 1

sub' :: String -> String -> Int -> String -> String
sub' [] [] _ new = new
sub' (a:q) [] ret new = sub' q [] (ret' (ord' a) ret) ((chr' ((ret' (ord' a) (ret)) * 10  + (ord' a) - ret)):new)
sub' [] (a':q') ret new = sub' [] q' (ret' 0 ((ord' a') + ret)) ((chr' ((ret' 0 ((ord' a') + ret)) * 10 - (ord' a') - ret)):new)
sub' (a:q) (a':q') ret new = sub' q q' (ret' (ord' a) ((ord' a') + ret)) ((chr' ((ret' (ord' a) ((ord' a') + ret)) * 10 + (ord' a) - (ord' a') - ret)):new)
--

--- Reverse String ---
--
reverse' :: String -> String -> String
reverse' [] new = new
reverse' (x:q) new = reverse' q (x:new)

revstr :: String -> String
revstr str = reverse' str []
--

--- Greater than ---
--
greaterthan' :: String -> String -> Bool -> Bool
greaterthan' [] [] x = x
greaterthan' (a':q') [] x = True
greaterthan' [] (a':q') x = False
greaterthan' (a:q) (a':q') x
  | a >= a' = greaterthan' q q' True
  | otherwise = greaterthan' q q' False

greaterthan :: String -> String -> Bool
greaterthan l1 l2 = greaterthan' l1 l2 True
--

--- Purge Zero ---
--
purgezero :: String -> String
purgezero (x:q)
  | x == '0' = purgezero q
  | otherwise = (x:q)
--

--- Gestion add/sub ---
--
--sub :: Bigint -> Bigint -> String
--sub (Bigint num1 Decimal Positive) (Bigint num2 Decimal Negative) = add' (revstr num1) (revstr num2) 0 []
--sub (Bigint num1 Decimal Negative) (Bigint num2 Decimal Positive) = add' (revstr num1) (revstr num2) 0 []
--sub (Bigint num1 Decimal Positive) (Bigint num2 Decimal Positive) = sub' (revstr num1) (revstr num2) 0 []
----sub (Bigint num1 Decimal Negative) (Bigint num2 Decimal Negative) = add' (revstr num1) (revstr num2) 0 []

add :: Bigint -> Bigint -> Bigint
add (Bigint num1 Decimal Positive) (Bigint num2 Decimal Positive)
  = Bigint (purgezero (add' (revstr num1) (revstr num2) 0 [])) Decimal Positive
add (Bigint num1 Decimal Negative) (Bigint num2 Decimal Negative)
  = Bigint (purgezero (add' (revstr num1) (revstr num2) 0 [])) Decimal Negative
add (Bigint num1 Decimal Positive) (Bigint num2 Decimal Negative)
  | (greaterthan num1 num2) == True
  = Bigint (purgezero (sub' (revstr num1) (revstr num2) 0 [])) Decimal Positive
  | otherwise = Bigint (purgezero (sub' (revstr num2) (revstr num1) 0 [])) Decimal Negative
add (Bigint num1 Decimal Negative) (Bigint num2 Decimal Positive)
  | (greaterthan num2 num1) == True
  = Bigint (purgezero (sub' (revstr num2) (revstr num1) 0 [])) Decimal Negative
  | otherwise = Bigint (purgezero (sub' (revstr num1) (revstr num2) 0 [])) Decimal Positive
--
