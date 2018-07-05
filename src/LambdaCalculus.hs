
{-
 - Copyright (c) Kove W. Ochre-Salter, 4th of July, 2018.
 - A simple library implementation of Alonzo Church's
 - Lambda Calculus.
 -}

module LambdaCalculus where

  import Data.List
  import Data.Like
  import Data.IState

  type Var = String

  data Expr :: * where
    Var :: Var  -> Expr
    Abs :: Var  -> Expr -> Expr
    App :: Expr -> Expr -> Expr
    deriving Show
  
  vars_             :: Expr -> [Var]
  vars_ (Var x)      = [x]
  vars_ (Abs x e)    = x : vars e
  vars_ (App e1 e2)  = vars e1 ++ vars e2

  vars :: Expr -> [Var]
  vars  = nub . vars_

  likevars       :: Expr -> Expr -> [Var]
  likevars e1 e2  = like (vars e1) (vars e2)

  rename                     :: Var -> Var -> Expr -> Expr
  rename old new (Var x)      = if x==old then Var new   else Var x
  rename old new (Abs x e)    = let n=rename old new e in if x==old then Abs new n else Abs x n
  rename old new (App e1 e2)  = App (rename old new e1) (rename old new e2)

  newname     :: Var -> Int -> Var
  newname v i  = "$" ++ v ++ show i

  genname   :: Var -> IState Var
  genname v  = S $ \state0 ->
    (newname v state0,state0)

  renames          :: [Var] -> Expr -> IState Expr
  renames []     e  = return e
  renames (v:vs) e  = do n <- genname v
                         e' <- renames vs e
                         return $ rename v n e'

  fixnames       :: Expr -> Expr -> IState Expr
  fixnames e1 e2  = do let lvs = likevars e1 e2
                       renames lvs e2

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

  eval            :: Expr -> IState [Expr]
  eval (Var x)     = return [Var x]
  eval (Abs p e)   = return [Abs p e]
  eval (App e1 e2) = do e1' <- eval e1
                        e2' <- fixnames e1 e2
                        return $ [e | e1'' <- e1',
                                      p    <- getparam e1'',
                                      e    <- rmparam (subst p e2' e1'')]

  run   :: Expr -> [Expr]
  run e  = let (es,_) = apply (eval e) 0
           in  es
