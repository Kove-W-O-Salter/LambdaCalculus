
{-
 - Copyright (c) Kove W. Ochre-Salter, 4th of July, 2018.
 - A simple library implementation of Alonzo Church's
 - Lambda Calculus.
 -}

module LambdaCalculus where

  type Var = Char

  data Expr :: * where
    Var :: Var  -> Expr
    Abs :: Var  -> Expr -> Expr
    App :: Expr -> Expr -> Expr
    deriving Show

  subst                      :: Var -> Expr -> Expr -> Expr
  subst name val (Var x)      = if x==name then val else Var x
  subst name val (App e1 e2)  = App (subst name val e1) (subst name val e2)
  subst name val (Abs p e)    = Abs p (subst name val e)

  getparam             :: Expr -> [Var]
  getparam (Var x)      = []
  getparam (Abs p e)    = [p]
  getparam (App e1 e2)  = []

  rmparam             :: Expr -> [Expr]
  rmparam (Var x)      = []
  rmparam (Abs p e)    = [e]
  rmparam (App e1 e2)  = []

  eval            :: Expr -> [Expr]
  eval (Var x)     = [Var x]
  eval (Abs p e)   = [Abs p e]
  eval (App e1 e2) = [e | e1' <- eval e1,
                          p   <- getparam e1',
                          e   <- rmparam (subst p e2 e1')]
