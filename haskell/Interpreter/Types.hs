module Interpreter.Types
       ( ExprC(..)
       , ExprS(..)
       , Value(..)
       , Name
       , Result(..)
       , Binding(..)
       , Env)
       where

type Name = String

data Result a = Result a Value

data Binding = Bind Name Value
               deriving(Show, Eq)

type Env = [Binding]

data Value = NumV Double |
             ClosV Name Env ExprC |
             BoolV Bool |
--             StringV String |
             ThunkV Env ExprC
             deriving(Show, Eq)
                     
data ExprC = IdC Name |
             BoolC Bool |
--             StringC String |
             IfC ExprC ExprC ExprC |
             NumC Double |
             PlusC ExprC ExprC |
             MultC ExprC ExprC |
             EqC ExprC ExprC |
             LamC Name ExprC |
             AppC ExprC ExprC |
             ThunkC ExprC |
             ForceC ExprC |
             ErrC String
             deriving(Show, Eq)

data ExprS = IdS Name |
             BoolS Bool |
--             StringS String |
             IfS ExprS ExprS ExprS |
             NumS Double |
             PlusS ExprS ExprS |
             MultS ExprS ExprS |
             EqS ExprS ExprS |
             LamS [Name] ExprS |
             AppS ExprS [ExprS] |
             ThunkS ExprS |
             ForceS ExprS |
             ErrS String |
             LetS [(String, ExprS)] ExprS
             deriving(Show, Eq)

