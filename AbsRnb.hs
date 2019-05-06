

module AbsRnb where

-- Haskell module generated by the BNF converter




newtype BTry = BTry ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BCatch = BCatch ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BLoop = BLoop ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BCase = BCase ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BAss = BAss ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BPlus = BPlus ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BMinus = BMinus ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BDiv = BDiv ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BMul = BMul ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BUpTo = BUpTo ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BMod = BMod ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BAnd = BAnd ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BOr = BOr ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype BNot = BNot ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BLBra = BLBra ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BLe = BLe ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype BLeEq = BLeEq ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BGr = BGr ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype BGrEq = BGrEq ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BNotEq = BNotEq ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BEq = BEq ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype BBool = BBool ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BReturn = BReturn ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BExit = BExit ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BContinue = BContinue ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BExitOn = BExitOn ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BIdent = BIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BInteger = BInteger ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BReal = BReal ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BChar = BChar ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype BString = BString ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
data Program = Start [Decl]
  deriving (Eq, Ord, Show, Read)

data Decl
    = VarDecl BIdent Type
    | VarDef BIdent Type BAss Expr
    | ConstDef BIdent Type BAss Expr
    | FuncDef BIdent [Param] BIdent Type [Stmt] BIdent
    | ProcDef BIdent [Param] [Stmt] BIdent
  deriving (Eq, Ord, Show, Read)

data Param = Parameter BIdent Mod Type
  deriving (Eq, Ord, Show, Read)

data Mod = ModRef | ModVal | ModConst | ModValRes
  deriving (Eq, Ord, Show, Read)

data Expr
    = ExprVal Val
    | ExprArray BLBra [Expr]
    | ExprCreate BLe Type BGr Expr
    | ExprAddress LeftExpr
    | ExprLeft LeftExpr
    | ExprCall BIdent [Expr]
    | ExprBrack Expr
    | ExprPostIncr LeftExpr
    | ExprPostDecr LeftExpr
    | ExprPow Expr BUpTo Expr
    | ExprMul Expr BMul Expr
    | ExprMod Expr BMod Expr
    | ExprDiv Expr BDiv Expr
    | ExprSum Expr BPlus Expr
    | ExprMinus Expr BMinus Expr
    | ExprUnaryMinus BMinus Expr
    | ExprUnaryPlus BPlus Expr
    | ExprAnd Expr BAnd Expr
    | ExprOr Expr BOr Expr
    | ExprNot BNot Expr
    | ExprRel Expr Rel Expr
    | ExprIf Expr Expr Expr
  deriving (Eq, Ord, Show, Read)

data Rel
    = RelLess BLe
    | RelLessEq BLeEq
    | RelGreater BGr
    | RelGreaterEq BGrEq
    | RelNotEq BNotEq
    | RelEq BEq
  deriving (Eq, Ord, Show, Read)

data Stmt
    = StmtAssign LeftExpr BAss Expr
    | StmtIf Expr [Stmt] ElseStmt
    | StmtLoop BLoop [Stmt] BLoop
    | StmtLoopCmd LoopCmd
    | StmtFor BIdent BType BAss Expr Dir Expr [Stmt]
    | StmtReturn BReturn
    | StmtCase BCase Expr Cases BCase
    | StmtLocal [Stmt]
    | StmtDecl Decl
    | StmtExpr Expr
    | StmtTryCatch BTry [Stmt] BCatch [Stmt] BCatch
  deriving (Eq, Ord, Show, Read)

data ElseStmt
    = StmtElseIf Expr [Stmt] ElseStmt | StmtElse [Stmt] | StmtNoElse
  deriving (Eq, Ord, Show, Read)

data Dir = FromTo | DownTo
  deriving (Eq, Ord, Show, Read)

data LoopCmd
    = LoopExitOn BExitOn Expr | LoopExit BExit | LoopContinue BContinue
  deriving (Eq, Ord, Show, Read)

data Cases
    = CaseEps | CaseDefault [Stmt] | CaseVal Expr [Stmt] Cases
  deriving (Eq, Ord, Show, Read)

data LeftExpr
    = LEId BIdent
    | LEArray LeftExpr Dim
    | LEBra LeftExpr
    | LEDeref LeftExpr
    | LEPreIncrem LeftExpr
    | LEPreDecr LeftExpr
  deriving (Eq, Ord, Show, Read)

data Dim = DimArray BLBra Expr
  deriving (Eq, Ord, Show, Read)

data Val
    = IntVal BInteger
    | RealVal BReal
    | CharVal BChar
    | BoolVal BBool
    | StringVal BString
  deriving (Eq, Ord, Show, Read)

data Type = BasicType BType | ArrayType BLe Type BGr | PointerType Type | ErrorType | VoidType | Array Type Int | ArrayDimType BLe Type BGr Expr
  deriving (Eq, Ord, Show, Read)

data BType = TypeInt | TypeReal | TypeChar | TypeBool | TypeString
    deriving (Eq, Ord, Show, Read)

