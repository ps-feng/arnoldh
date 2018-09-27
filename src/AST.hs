module AST where

type Program = [Statement]

data ArithBinaryOp
  = Add
  | Minus
  | Mult
  | Divide
  deriving (Show)

data ArithExpr
  = Var String
  | IntConst Integer
  | ArithBinary ArithBinaryOp
                ArithExpr
                ArithExpr
  deriving (Show)

data PrintExpr
  = Print String
  | PrintVar String
  deriving (Show)

data IntVarDecl =
  IntVar String
         Integer
  deriving (Show)

data Statement
  = Assignment String
               ArithExpr
  | PrintExpr
  | IntVarDecl
  deriving (Show)
