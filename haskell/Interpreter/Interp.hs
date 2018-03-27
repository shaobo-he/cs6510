module Interpreter.Interp
       (interp, interpS)
       where

import Interpreter.Types
import Interpreter.Parser
import Interpreter.Desugar
import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

type EValue = ExceptT String Identity Value
type REValue = ReaderT Env (ExceptT String Identity) Value

interpS :: String -> Either String Value
interpS s = case parseLisp s of
              Left err -> Left $ "Parse err: " ++ show err
              Right v  -> runIdentity
                        $ runExceptT
                        $ flip runReaderT []
                        $ interp (desugar v)

ilookup :: Name -> Env -> Maybe Value
ilookup n [] = Nothing

ilookup n ((Bind bn bv):bs) = if   bn == n
                              then Just bv
                              else ilookup n bs

num_op :: (Double->Double->b) -> (b->Value) -> Value -> Value -> REValue
num_op f cnst l r = case (l, r) of
                      ((NumV left), (NumV right)) -> return $ cnst $ f left right
                      _ -> throwError "not a number"

num_plus = num_op (+) NumV
num_mult = num_op (*) NumV
num_eq   = num_op (==) BoolV

extend_env :: Binding -> Env -> Env
extend_env = (:)

interp :: ExprC -> REValue
interp exp =
  case exp of
    IdC name -> do
                e <- ask
                case (ilookup name e) of
                  Just v' -> return v'
                  Nothing -> throwError $ name ++ ": name not found"
    NumC v -> return $ NumV v
    BoolC p -> return $ BoolV p
    PlusC l r -> join $ num_plus <$> interp l <*> interp r
    MultC l r -> join $ num_mult <$> interp l <*> interp r
    EqC l r -> join $ num_eq <$> interp l <*> interp r
    IfC c th el -> do
                     e <- ask
                     cond <- interp c
                     case cond of
                       BoolV True -> interp th
                       BoolV False -> interp el
                       _ -> throwError "not a boolean"
    LamC n body -> do
                     e <- ask
                     return $ ClosV n e body
    AppC fun arg -> do
                      f <- interp fun
                      a <- interp arg
                      case f of
                        ClosV name env body -> withReaderT (const (extend_env (Bind name a) env)) $ interp body
                        _ -> throwError "not a function"
    ThunkC exp -> do
                    e <- ask
                    return $ ThunkV e exp
    ForceC exp -> do
                    f <- interp exp
                    case f of
                      ThunkV tenv texp -> withReaderT (const tenv) $ interp texp
                      _ -> throwError "not a thunk"
    ErrC msg -> error msg


