module Grammar where

type Program = [Stmt]

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

data Stmt =
  Assignment String
             ArithExpr
