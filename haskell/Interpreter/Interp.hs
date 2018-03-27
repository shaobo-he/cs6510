module Interpreter.Interp
       (interp, interpS)
       where

import Interpreter.Types
import Interpreter.Parser
import Interpreter.Desugar
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Except

interpS :: String -> Either String Value
interpS s = case parseLisp s of
              Left err -> Left $ "Parse err: " ++ show err
              Right v  -> Right $ runIdentity $ interp (desugar v) []

ilookup :: Name -> Env -> Maybe Value
ilookup n [] = Nothing

ilookup n ((Bind bn bv):bs) = if   bn == n
                              then Just bv
                              else ilookup n bs

num_op f cnst l r = case (l, r) of
                      ((NumV left), (NumV right)) -> cnst $ f left right
                      _ -> error "not a number"

num_plus = num_op (+) NumV
num_mult = num_op (*) NumV
num_eq   = num_op (==) BoolV

extend_env :: Binding -> Env -> Env
extend_env = (:)

interp :: ExprC -> Env -> Identity Value
interp exp e =
  case exp of
    IdC name -> case (ilookup name e) of
                  Just v' -> return v'
                  Nothing -> error $ name ++ ": name not found"
    NumC v -> return $ NumV v
    BoolC p -> return $ BoolV p
    PlusC l r -> num_plus <$> interp l e <*> interp r e
    MultC l r -> num_mult <$> interp l e <*> interp r e
    EqC l r -> num_eq <$> interp l e <*> interp r e
    IfC c th el -> do
                     cond <- interp c e
                     case cond of
                       BoolV True -> interp th e
                       BoolV False -> interp el e
                       _ -> error "not a boolean"
    LamC n body -> return $ ClosV n e body
    AppC fun arg -> do
                      f <- interp fun e
                      a <- interp arg e
                      case f of
                        ClosV name env body -> interp body $
                                extend_env (Bind name a) env
                        _ -> error "not a function"
    ThunkC exp -> return $ ThunkV e exp
    ForceC exp -> do
                    f <- interp exp e
                    case f of
                      ThunkV tenv texp -> interp texp tenv
                      _ -> error "not a thunk"
    ErrC msg -> error msg


