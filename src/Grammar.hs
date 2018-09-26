module Grammar where

type Program = [Statement]

data ArithBinaryOp
  = Add
  | Minus
  | Mult
  | Divide

data ArithExpr
  = Var String
  | IntConst Integer
  | ArithBinary ArithBinaryOp
                ArithExpr
                ArithExpr

data Statement =
  Assignment String
             ArithExpr
