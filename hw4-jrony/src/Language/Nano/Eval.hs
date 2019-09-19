{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser
import Debug.Trace


--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- 0
--
-- >>> eval env0 "p"
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (2 : [])

--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval  xs  (ENil)       =  VNil
eval  xs  (EInt i )    =  VInt i
eval  xs  (EBool b)    =  VBool b
eval (xs) (EVar id)    =  lookupId id (xs)
eval (xs) (EBin g h i) =  evalOp g (eval (xs) h) (eval (xs) i)
eval  xs  (EIf  g h i)
                     |(eval (xs) g) == (VBool True)  = (eval (xs) h)
                     |(eval (xs) g) == (VBool False) = (eval (xs) i)
                     |otherwise                      = VErr (throw (Error ("type error")))



eval env (ELet x e1 e2) = eval env' e2
  where




    v    = eval env' e1
    env' = [(x, v)] ++ env      -- e1 needs to KNOW ITSELF when e2 is evaluated.  This is weird af, but it works.
                                -- v equals the evaluation of e1 with the environment that x (the i.d.; you can think of it as "fac")
                                -- is mapped to the evaluation of itself in.


eval xs (ELam (id) e) = VClos xs id e
eval xs (EApp e1 e2) = case (eval xs e1) of
                       (VClos env id expr)         -> eval ([(id, (eval xs e2))] ++ env) expr
                       (VPrim(func))               -> func (eval xs e2)



                       u                           -> VErr (throw (Error ("type error EApp" ++ show u)))





--eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0)) (EInt 1) (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1)))))) (EApp "fac" (EInt 10)))

--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus  (VInt y) (VInt z)     = VInt (y + z)
evalOp Plus  (_)      (_)          = VErr (throw (Error "type error"))

evalOp Minus (VInt y) (VInt z)     = VInt (y - z)
evalOp Minus (_)      (_)          = VErr (throw (Error "type error"))

evalOp Mul   (VInt y) (VInt z)     = VInt (y * z)
evalOp Mul   (_)      (_)          = VErr (throw (Error "type error"))

evalOp Div   (VInt y) (VInt z)     = VInt (y `div` z)
evalOp Div   (_)      (_)          = VErr (throw (Error "type error"))


evalOp Eq   (VPair x y) (VPair a b) = if headl(VPair x y) == headl(VPair a b)  --List Equality
                                      then evalOp Eq (taill(VPair x y)) (taill(VPair a b))
                                      else VBool False
evalOp Eq    (VInt y)  (VInt z)     = if y == z
                                      then VBool True
                                      else VBool False
evalOp Eq    (VBool y) (VBool z)    = if y == z
                                      then VBool True
                                      else VBool False
evalOp Eq    (VNil) (VNil)          = VBool True
evalOp Eq     _     VNil            = VBool False
evalOp Eq     VNil     _            = VBool False
evalOp Eq    (_)       (_)          = VErr (throw (Error "type error"))

evalOp Ne (VPair x y) (VPair a b)  = if headl(VPair x y) == headl(VPair a b) --Negation of List equality
                                     then evalOp Ne (taill(VPair x y)) (taill(VPair a b))
                                     else VBool True
evalOp Ne    (VInt y)  (VInt z)    = if y /= z
                                     then VBool True
                                     else VBool False
evalOp Ne    (VBool y) (VBool z)   = if y /= z
                                     then VBool True
                                     else VBool False
evalOp Ne    (VNil) (VNil)         = VBool False
evalOp Ne      _    (VNil)         = VBool True
evalOp Ne    (VNil)   _            = VBool True
evalOp Ne       _     _            = VErr (throw (Error "type error"))

evalOp Le    (VInt y) (VInt z)     = if y <= z
                                     then VBool True
                                     else VBool False
evalOp Le    (_)      (_)          = VErr (throw (Error "type error"))

evalOp Lt    (VInt y) (VInt z)     = if y < z
                                     then VBool True
                                     else VBool False
evalOp Lt    (_)      (_)          = VErr (throw (Error "type error"))


evalOp And   (VBool y) (VBool z)   = if y == True && z == True
                                     then VBool True
                                     else VBool False
evalOp And   (_)       (_)         = VErr (throw (Error "type error"))

evalOp Or    (VBool y) (VBool z)
                               |y == True && z == False = VBool True
                               |y == False && z == True = VBool True
                               |y == True && z == True = VBool True
                               |y == False && z == False = VBool False
evalOp Or    (_)       (_)         = VErr (throw (Error "type error"))


evalOp Cons (VPair x VNil) (VPair a b) = VPair x (VPair a b)

evalOp Cons (VPair x y)    z           = case z of
                                          (VPair a b) -> VPair x (evalOp Cons (y) (VPair a b) )
                                          otherwise   -> VErr (throw (Error "type error"))

evalOp Cons   x         y              = VPair x y









--eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq (EVar "n") (EInt 0)) (EInt 1) (EBin Mul (EVar "n") (EApp (EVar "fac") (EBin Minus (EVar "n") (EInt 1)))))) (EApp (EVar "fac") (EInt 10)))

--eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq (EVar "n") (EInt 0)) (EInt 1) (EBin Mul (EVar "n") (EApp (EVar "fac") (EBin Minus (EVar "n") (EInt 1)))))) (EApp (EVar "fac") (EInt 10)))

                                                                             --error "TBD:evalOp"

--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId k []     =  VErr (throw (Error ("type error: unbound variable " ++ k)))
lookupId k (x:xs) = if fst(x) == k
                    then snd(x)
                    else lookupId k xs

--------------------------------------------------------------------------------
headl :: Value -> Value
--------------------------------------------------------------------------------
headl (VPair x y) = x
headl _           = VErr (throw (Error ("type error")))

--------------------------------------------------------------------------------
taill :: Value -> Value
--------------------------------------------------------------------------------
taill (VPair x y) = y
taill _           = VErr (throw (Error ("type error")))

--------------------------------------------------------------------------------
prelude :: Env
--------------------------------------------------------------------------------
prelude = [("head", (VPrim (headl))), ("tail", (VPrim (taill)))]

                                                                                            -- HINT: you may extend this "built-in" environment
                                                                                               -- with some "operators" that you find useful...

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
