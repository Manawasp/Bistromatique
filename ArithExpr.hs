module ArithExpr (sun') where

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

sun' x y = x + y
