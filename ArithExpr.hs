module ArithExpr (Arith_expr(Number, Mult, Div, Add, Sub, Mod), string_of_arith_expr) where

import Bigint

-- __DATA__ --

--- Tree of expression ---
data Arith_expr
  = Number Bigint
  | Mult [Arith_expr]
  | Div  [Arith_expr]
  | Add  [Arith_expr]
  | Sub  [Arith_expr]
  | Mod  [Arith_expr]

-- !__DATA__ --

string_of_arith_expr :: Arith_expr -> String
string_of_arith_expr (Number x) = string_of_bigint x


sun' x y = x + y
