--  __DATA__ --

--- infinite number ---
data Base = Binary | Octal | Decimal | Hexadecimal
data Sign = Positive | Negative
data Bigint = Bigint String Base Sign

--- Tree of expression ---
data Arith_expr
  = Number Bigint
  | Mult [Arith_expr]
  | Div  [Arith_expr]
  | Add  [Arith_expr]
  | Sub  [Arith_expr]
  | Mod  [Arith_expr]

--- Lexer/Parser ---
data Token = POP | PED | OP | Numb
data Lexer
  = Empty
  | Lex Token Lexer

--  !__DATA__ --

--- view Data  --
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
get_num ('0':q) = get_num q
get_num str = str

bigint_of_string :: String -> Bigint
bigint_of_string str = Bigint (get_num str) (get_base str) (get_sign str)

--
