--
---- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
---- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
----
import System.Environment
import Bigint
import ArithExpr

-- -- | 'main' runs the main program
--main :: IO ()
--main = getArgs >>= print . haqify . head

v = bigint_of_string "-98989"



--haqify s = s
main = do
        putStr "Chose a first number ?\n"
        number1 <- getLine
        putStr "Chose a second number ?\n"
        number2 <- getLine
        let bx = bigint_of_string number1
        let by = bigint_of_string number2
        let list = [Number bx, Number by]
        let term = Add list
        putStr ((string_of_bigint bx)
                ++ "+" ++
                (string_of_bigint by) ++ "\n")
        let addType = add bx by
        putStr ("resultat : " ++ (string_of_bigint addType) ++ "\n")
