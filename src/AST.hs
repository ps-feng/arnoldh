module AST where

type Program = [AbstractMethod]
type MethodName = String
type VarName = String

data Op
  = Add
  | Minus
  | Mult
  | Divide
  | Modulo
  | Or
  | And
  | EqualTo
  | GreaterThan
  deriving (Show, Eq)

data Expr
  = Int Integer
  | Var VarName
  | BinaryOp Op
             Expr
             Expr
  deriving (Show, Eq)

data Statement
  = Assignment VarName
               Expr
  | PrintExpr Expr
  | PrintStr String
  | IntVar VarName Expr
  | If Expr
       [Statement]
       [Statement]
  | While Expr
          [Statement]
  | CallMethod (Maybe VarName) MethodName [Expr]
  | CallRead VarName -- not sure if VarName should be optional
  | Return (Maybe Expr)
  deriving (Show, Eq)

data MethodArg =
  MethodArg String
  deriving (Show, Eq)

data AbstractMethod
  = Main [Statement]
  | Method MethodName
           [MethodArg]
           [Statement]
  deriving (Show, Eq)
