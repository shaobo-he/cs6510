import Interpreter.Interp
import Interpreter.Types

tests = [("(+ 1 2)", (NumV 3.0)),
         ("(let ([x 3.0]\
                \ [y 4.0]\
                \ [z 5.0])\
           \ (+ x (* y z)))", (NumV 23.0)),
         ("((lambda (x y z) (* x (* y z))) 2.0 3.0 4.0)", (NumV 24.0))]


runtest :: (String, Value) -> Bool
runtest (exp, expected) = case interpS exp of
                          Left err -> False
                          Right v -> v == expected

main =
  if and $ map runtest tests
  then putStrLn "Tests passed"
  else putStrLn "Test failed"
