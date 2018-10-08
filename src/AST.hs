module AST where

type Program = [AbstractMethod]

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
  | String String -- check also Data.Text
  | Var String
  | BinaryOp Op
             Expr
             Expr
  deriving (Show, Eq)

data Statement
  = Assignment String
               Expr
  | Print Expr
  | IntVar Integer -- TODO: 16bit signed
  | If Expr
       [Statement]
       [Statement]
  | While Expr
          [Statement]
  | CallRead String
  | Return (Maybe Expr)
  deriving (Show, Eq)

data MethodArg =
  MethodArg String
  deriving (Show, Eq)

data AbstractMethod
  = Main [Statement]
  | Method String
           [MethodArg]
           [Statement]
  deriving (Show, Eq)
