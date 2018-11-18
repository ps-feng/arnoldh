module AST where

import qualified Region as R

type Program = [AbstractMethod]
type LocatedMethodName = R.Located String
type LocatedMethodArg = R.Located MethodArg
type LocatedVarName = R.Located String
type LocatedExpr = R.Located Expr
type LocatedStatement = R.Located Statement
type LocatedMethod = R.Located AbstractMethod

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
  | Var String
  | BinaryOp Op
             LocatedExpr
             LocatedExpr
  deriving (Show, Eq)

data Statement
  = Assignment LocatedVarName
               LocatedExpr
  | PrintExpr LocatedExpr
  | PrintStr String
  | IntVar LocatedVarName
           LocatedExpr
  | If LocatedExpr
       [LocatedStatement]
       [LocatedStatement]
  | While LocatedExpr
          [LocatedStatement]
  | CallMethod (Maybe LocatedVarName)
               LocatedMethodName
               [LocatedExpr]
  | CallRead LocatedVarName -- not sure if VarName should be optional
  | Return (Maybe LocatedExpr)
  deriving (Show, Eq)

data MethodArg =
  MethodArg String
  deriving (Show, Eq, Ord)

data ReturnType
  = TVoid
  | TInt
  deriving (Show, Eq)

data AbstractMethod
  = Main [LocatedStatement]
  | Method LocatedMethodName
           ReturnType
           [LocatedMethodArg]
           [LocatedStatement]
  deriving (Show, Eq)
