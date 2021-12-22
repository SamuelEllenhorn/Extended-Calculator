module Interpreter where

import AbsNumbers

eval :: Exp -> Integer
eval (Num n) = n
eval (Plus n m) = (eval n) +  (eval m)
eval (Sub n m) = (eval n) - (eval m)
eval (Div n m) = (eval n) / (eval m)
eval (Times n m) = (eval n) * (eval m)
eval (Exp n m) = (eval n) ^ (eval m)
