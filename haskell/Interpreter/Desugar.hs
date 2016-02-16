module Interpreter.Desugar
       (desugar)
       where

import Interpreter.Types

desugar :: ExprS -> ExprC
desugar e = case e of
  IdS name -> IdC name
  BoolS p -> BoolC p
  IfS c t e -> IfC (desugar c) (desugar t) (desugar e)
  LamS ns body -> foldr (\n lam -> (LamC n lam)) (desugar body) ns
  NumS n -> NumC n
  PlusS l r -> PlusC (desugar l) (desugar r)
  MultS l r -> MultC (desugar l) (desugar r)
  EqS l r -> EqC (desugar l) (desugar r)
  AppS f as -> foldl (\app a -> (AppC app a)) (desugar f) (map desugar as)
  ThunkS e -> ThunkC (desugar e)
  ForceS e -> ForceC (desugar e)
  ErrS m -> ErrC m
  LetS nvps body -> desugar $ (AppS
                               (LamS [ n | (n, _) <- nvps ] body)
                               [ a | (_, a) <- nvps ])
  
