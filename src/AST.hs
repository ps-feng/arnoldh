module AST where

type Program = [Statement]

data ArithBinaryOp
  = Add
  | Minus
  | Mult
  | Divide
  deriving (Show, Eq)

data ArithExpr
  = Var String
  | IntConst Integer
  | ArithBinary ArithBinaryOp
                ArithExpr
                ArithExpr
  deriving (Show, Eq)

data PrintExpr
  = Print String
  | PrintVar String
  deriving (Show, Eq)

data IntVarDecl =
  IntVar String
         Integer
  deriving (Show, Eq)

data Statement
  = Assignment String
               ArithExpr
  | PrintExpr
  | IntVarDecl
  deriving (Show, Eq)
