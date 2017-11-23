module Ast where





data Exp t =
  Variable String t
  | IntegerConst t
  | Sum (Exp t) (Exp t)
  | Less (Exp t) (Exp t)
  | And (Exp t) (Exp t)
  | Not (Exp t)

  
type Env t = t -> Integer

--eval

eval :: Integral t => Env t -> Exp t -> Integer

eval (var) (IntegerConst t) = toInteger t
eval (var) (Variable name t) = (var t)
eval (var) (Sum t1 t2)  = (eval (var) (t1)) + (eval (var) (t2)) 

--eval for boolean expressions

eval (var) (Less t1 t2) = if (eval (var) (t1)) < ( eval (var) (t2) )  then 1 else 0
eval (var) (And t1 t2) = if (eval (var) (t1)) /= 0 && (eval (var) (t2)) /= 0  then 1 else 0
eval (var) (Not t1) = if (eval (var) (t1)) /= 0 then 0 else 1

--making the how thing Showable

instance (Show t)=>(Show (Exp t) ) where
  show (IntegerConst t) = show t
  show (Variable name t) = name
  show (Sum t1 t2) = "(" ++ show t1 ++" + "++ show t2 ++ ")"
  show (Less t1 t2) = "(" ++ show t1 ++ " < " ++ show t2 ++ ")"
  show (And t1 t2) = "(" ++ show t1 ++ " & " ++ show t2 ++ ")"
  show (Not t) = "!" ++ show t



dr1 = And (IntegerConst 8) (IntegerConst 0)
dr2 = Sum (IntegerConst 8) (IntegerConst 0)
dr3 = Less (IntegerConst 8) (IntegerConst 0)
dr4 = And (Not (Variable "x" 5) ) (dr3)


