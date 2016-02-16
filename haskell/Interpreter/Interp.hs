module Interpreter.Interp
       (interp, interpS)
       where

import Interpreter.Types
import Interpreter.Parser
import Interpreter.Desugar

interpS :: String -> Either String Value
interpS s = case parseLisp s of
              Left err -> Left $ "Parse err: " ++ show err
              Right v  -> Right $ interp (desugar v) []

ilookup :: Name -> Env -> Value
ilookup n [] = error $ n ++ ": name not found"

ilookup n ((Bind bn bv):bs) = if   bn == n
                              then bv
                              else ilookup n bs

num_op f cnst l r = case (l, r) of
                      ((NumV left), (NumV right)) -> cnst $ f left right
                      _ -> error "not a number"

num_plus = num_op (+) NumV
num_mult = num_op (*) NumV
num_eq   = num_op (==) BoolV

extend_env :: Binding -> Env -> Env
extend_env = (:)

interp :: ExprC -> Env -> Value
interp exp e =
  case exp of
    IdC name -> ilookup name e
    NumC v -> NumV v
    BoolC p -> BoolV p
    PlusC l r -> let lv = interp l e
                     rv = interp r e
                 in  num_plus lv rv
    MultC l r -> let lv = interp l e
                     rv = interp r e
                 in  num_mult lv rv
    EqC l r -> let lv = interp l e
                   rv = interp r e
               in  num_eq lv rv
    IfC c th el -> let cond = interp c e
                   in case cond of
                        BoolV True -> interp th e
                        BoolV False -> interp el e
                        _ -> error "not a boolean"
    LamC n body -> ClosV n e body
    AppC fun arg -> case interp fun e of
                        ClosV name env body
                          -> interp body $
                                    extend_env (Bind name $ interp arg e) env
                        _ -> error "not a function"
    ThunkC exp -> ThunkV e exp
    ForceC exp -> case interp exp e of
                    ThunkV tenv texp -> interp texp tenv
                    _ -> error "not a thunk"
    ErrC msg -> error msg


