
module Printing (showExp) where

import Exp

showVar :: Var -> String
showVar = getVar 

showExp :: ComplexExp -> String
showExp (CX var) = showVar var
showExp (CLam var exp) = "(\\" ++ showVar var ++ " -> " ++ showExp exp ++ ")" 
showExp (CApp exp1 exp2) = "(" ++ showExp exp1 ++ " " ++ showExp exp2 ++ ")"
