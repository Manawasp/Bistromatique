--  __DATA__ --

--- infinite number ---
data Base = Binary | Octal | Decimal | Hexadecimal
data Sign = Positive | Negative
data Bigint = String Base Sign

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
sum str = str
bigint_of_string str = str
