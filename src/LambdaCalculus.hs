
{-
 - Copyright (c) Kove W. Ochre-Salter, 4th & 5th of July, 2018.
 - A simple library implementation of Alonzo Church's
 - Lambda Calculus.
 -}

module LambdaCalculus where

  import Data.List
  import Data.Like
  import Data.IState

  {-
   - Variable.
   -}
  type Var = String

  {-
   - Semantic representation of expressions.
   -}
  data Expr :: * where
    Var :: Var  -> Expr          -- Variable
    Abs :: Var  -> Expr -> Expr  -- Abstraction
    App :: Expr -> Expr -> Expr  -- Application
    deriving Show
  
  {-
   - Find all the vars in an expression.
   -}
  vars_             :: Expr -> [Var]
  vars_ (Var x)      = [x]
  vars_ (Abs x e)    = x : vars e
  vars_ (App e1 e2)  = vars e1 ++ vars e2

  {-
   - Find all the unique vars in an expression.
   -}
  vars :: Expr -> [Var]
  vars  = nub . vars_

  {-
   - Find all the like-vars in two expressions.
   -}
  likevars       :: Expr -> Expr -> [Var]
  likevars e1 e2  = like (vars e1) (vars e2)

  {-
   - Replace all instances of a var with a new var.
   -}
  rename                     :: Var -> Var -> Expr -> Expr
  rename old new (Var x)      = if x==old then Var new   else Var x
  rename old new (Abs x e)    = let n=rename old new e in if x==old then Abs new n else Abs x n
  rename old new (App e1 e2)  = App (rename old new e1) (rename old new e2)

  {-
   - Create a new name for a var.
   -}
  newname     :: Var -> Int -> Var
  newname v i  = "$" ++ v ++ show i

  {-
   - Generate a new unique name for a var.
   -}
  genname   :: Var -> IState Var
  genname v  = S $ \state0 ->
    (newname v state0,state0)

  {-
   - Generate new names a list of vars and rename
   - the old names with the new names in an expression.
   -}
  renames          :: [Var] -> Expr -> IState Expr
  renames []     e  = return e
  renames (v:vs) e  = do n <- genname v
                         e' <- renames vs e
                         return $ rename v n e'

  {-
   - Rename all duplicate names in two expressions
   - to avoid name confliction when they are combined.
   -}
  fixnames       :: Expr -> Expr -> IState Expr
  fixnames e1 e2  = do let lvs = likevars e1 e2
                       renames lvs e2

  {-
   - Substitute all usages of a var with an expression.
   -}
  subst                      :: Var -> Expr -> Expr -> Expr
  subst name val (Var x)      = if x==name then val else Var x
  subst name val (App e1 e2)  = App (subst name val e1) (subst name val e2)
  subst name val (Abs p e)    = Abs p (subst name val e)

  {-
   - Get the first paramater in an Abstraction.
   -}
  getparam             :: Expr -> [Var]
  getparam (Var x)      = []
  getparam (Abs p e)    = [p]
  getparam (App e1 e2)  = []

  {-
   - Remove the first paramater of an Abstraction.
   -}
  rmparam             :: Expr -> [Expr]
  rmparam (Var x)      = []
  rmparam (Abs p e)    = [e]
  rmparam (App e1 e2)  = []

  {-
   - Evaluate an expression.
   -}
  eval            :: Expr -> IState [Expr]
  eval (Var x)     = return [Var x]
  eval (Abs p e)   = return [Abs p e]
  eval (App e1 e2) = do e1' <- eval e1
                        e2' <- fixnames e1 e2
                        return $ [e | e1'' <- e1',
                                      p    <- getparam e1'',
                                      e    <- rmparam (subst p e2' e1'')]

  {-
   - Run an expression.
   -}
  run   :: Expr -> [Expr]
  run e  = let (es,_) = apply (eval e) 0
           in  es
