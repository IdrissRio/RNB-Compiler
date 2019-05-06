module Utils where

import Data.ByteString.Char8 (unpack, ByteString, pack) 
import Data.Map (Map, member, insert, lookup)
import qualified Data.Map as Map
import Data.Char
import AbsRnb
import Data.List

--------------------------------------------------------------------
------------------------- DATA TYPES -------------------------------
--------------------------------------------------------------------

type Pos = (Int, Int)

type Id = String

data TACAddr  = Addr String | PointAddr String
  | DerefAddr String | AddrAt String TACAddr
  | ValAddr String
  | NoAddr
  deriving (Show, Eq)
data TACLabel = Label String | Fall | NoLab         deriving (Show, Eq)
type TACOp = String

data TAC
  = TACAssign TACAddr Type TACAddr                -- a1 = a2
  | TACUnOp TACAddr TACOp Type TACAddr            -- a1 = op a2
  | TACBinOp TACAddr TACAddr TACOp Type TACAddr   -- a1 = a2 op a3
  | TACGoto TACLabel                              -- goto l
  | TACAddLabel TACLabel                          -- l :
  | TACIf TACAddr TACLabel                        -- if a goto l
  | TACIfFalse TACAddr TACLabel                   -- ifFalse a goto l
  | TACIfRel TACAddr Rel Type TACAddr TACLabel    -- if a1 op a2 goto l
  | TACParam TACAddr                              -- param a
  | TACCallProc TACLabel                          -- call p / n
  | TACCallFunc TACAddr Type TACLabel             -- a = call f / n
  | TACReturn TACAddr                             -- return a
  | TACException TACLabel                         -- onexceptiongoto l
  | TACExit                                       -- halt
  | TACComment String                             -- // comment
  | TACStatic TACAddr String                      -- straddr string
  | TACCode
  | TACStaticData
  deriving (Show)


--------------------------------------------------------------------
-------------------------  HELPERS ---------------------------------
--------------------------------------------------------------------

--------------------------------------------------------------------
--------------------------- TYPES ----------------------------------

leqType :: Type -> Type -> Bool
leqType ty1 ty2 =
    case ty1 of
        BasicType b1 ->
            case ty2 of
                BasicType b2 -> leqBType b1 b2 
                ErrorType    -> True
                otherwise    -> False
        Array aty1 adim1 ->
            case ty2 of
                Array aty2 adim2 -> (leqType aty1 aty2 && adim2==adim1) ||
                                    containsError aty1 || containsError aty2
                ErrorType    -> True
                otherwise    -> False
        PointerType b1 ->
            case ty2 of
                PointerType b2 ->  b1 == b2
                ErrorType    -> True
                otherwise      -> False
        ErrorType -> True
        VoidType -> False

containsError :: Type -> Bool
containsError ty =
  case ty of
      ErrorType   -> True
      Array tty _ -> containsError tty
      otherwise   -> False


equalitySupport :: Type -> Bool
equalitySupport typeE1 = case typeE1 of 
                                ArrayType _ _ _ -> False
                                Array _ _ -> False
                                PointerType _ -> False
                                otherwise -> True

leqBType :: BType -> BType -> Bool
leqBType ty1 ty2 | ty1 == ty2 = True
                 | ty1 == TypeInt && ty2 == TypeReal = True
                 | ty1 == TypeChar && ty2 == TypeInt = True
                 | ty1 == TypeChar && ty2 == TypeString = True
                 | ty1 == TypeChar && ty2 == TypeReal = True
                 | otherwise = False

getMaxBType :: BType -> BType -> BType
getMaxBType ty1 ty2 | leqBType ty1 ty2 = ty2
                    | leqBType ty2 ty2 = ty1

getMaxType :: Type -> Type -> Type
getMaxType ty1 ty2 | leqType ty1 ty2 = ty2
                   | leqType ty2 ty2 = ty1

areCompatibleTypes :: Type -> Type -> Bool
areCompatibleTypes ty1 ty2 | leqType ty1 ty2 = True
                           | leqType ty2 ty1 = True
                           | otherwise = False

isAddressType :: Type -> Bool
isAddressType (BasicType TypeString) = True
isAddressType (PointerType _) = True
isAddressType (Array _ _) = True
isAddressType _ = False

derefType :: Type -> Type
derefType (PointerType ty) = ty

pointerToType :: Type -> Type
pointerToType ty = PointerType ty

getArrayElemType :: Type -> Type
getArrayElemType (Array ty _) = ty
getArrayElemType t = error (show t)

getBasicType :: Type -> Type
getBasicType ty = case ty of
  BasicType _ -> ty
  ArrayType _ t _ -> getBasicType t
  ArrayDimType _ t _ _ -> getBasicType t
  PointerType t -> getBasicType t

isBasicType :: Type -> Bool
isBasicType (BasicType _) = True
isBasicType _ = False

areErrors :: Type -> Type -> Bool
areErrors ty1 ty2 = isError ty1 || isError ty2

isError :: Type -> Bool
isError ty1  = (ty1 == ErrorType) || case ty1 of 
                                     Array ErrorType _ ->True
                                     otherwise -> False

numericalComp :: Type -> Bool
numericalComp ty1
          | areCompatibleTypes (BasicType TypeInt) ty1 &&
            ty1 /= ErrorType  = True
          | otherwise = False

comparableTypes :: Type -> Type -> Bool
comparableTypes ty1 ty2
            | areCompatibleTypes ty1 ty2  &&
              isBTypeTy1 && isBTypeTy2 = True
            | ty1 == (BasicType TypeChar) && ty2 == ty1 = True
            | otherwise = False
            where isBTypeTy1 = case ty1 of
                            (BasicType TypeInt) -> True
                            (BasicType TypeReal) -> True
                            (BasicType TypeChar) -> True
                            otherwise -> False
                  isBTypeTy2 = case ty2 of
                            (BasicType TypeInt) -> True
                            (BasicType TypeReal) -> True
                            (BasicType TypeChar) -> True
                            otherwise -> False


operationReturnType :: Type -> Type -> String -> Type
operationReturnType ty1 ty2 op
    | op == "/" = BasicType TypeReal
    | op == "^" && (leqType ty1 (BasicType TypeInt)) &&
                   (leqType ty2 (BasicType TypeInt))
                   = BasicType TypeInt
    | op == "%" && (leqType ty1 (BasicType TypeInt)) &&
                   (leqType ty2 (BasicType TypeInt))
                   = BasicType TypeInt
    | op == "%" = ErrorType
    | op == "^" = ErrorType
    | op == "+" = getMaxType (getMaxType ty1 ty2) (BasicType TypeInt)
    | op == "-" = getMaxType (getMaxType ty1 ty2) (BasicType TypeInt)
    | op == "*" = getMaxType (getMaxType ty1 ty2) (BasicType TypeInt)
    | otherwise = getMaxType ty1 ty2 

--------------------------

sizeOfType :: Type -> Int
sizeOfType ty = case ty of
  BasicType TypeInt -> 4
  BasicType TypeReal -> 8
  BasicType TypeChar -> 2
  BasicType TypeBool -> 1
  BasicType TypeString -> 4
  PointerType _ -> 4
  Array t n -> n * (sizeOfType t)

expr_sizeOfType :: Type -> Expr
expr_sizeOfType ty = ExprVal $ IntVal $ BInteger ((0,0), v)
  where v = show $ sizeOfType ty

unityAddr :: Type -> TACAddr
unityAddr ty = case ty of
  BasicType TypeInt -> ValAddr "1"
  BasicType TypeReal -> ValAddr "1.0"

---------------------------

defaultValue :: Type -> Expr
defaultValue ty = case ty of
  BasicType TypeInt -> ExprVal $ IntVal $ BInteger ((0,0),"0")
  BasicType TypeReal -> ExprVal $ RealVal $ BReal ((0,0),"0.0")
  BasicType TypeChar -> ExprVal $ CharVal $ BChar ((0,0),"'\\u0000'")
  BasicType TypeBool -> ExprVal $ BoolVal $ BBool ((0,0),"False")
  BasicType TypeString -> ExprVal $ StringVal $ BString ((0,0), "\"\"")

defaultValsList :: Type -> Int -> [Expr]
defaultValsList _ n | n <= 0 = [] -- safety check
defaultValsList ty n = take n (repeat $ defaultValue ty)


--------------------------------------------------------------------
--------------------------- VALUES ---------------------------------

getIntValue :: TACAddr -> Int
getIntValue (ValAddr v) = read v

getRealValue :: TACAddr -> Double
getRealValue (ValAddr v) = read v :: Double

getCharValue :: TACAddr -> String
getCharValue (ValAddr v) = v

--------------

maybeConvertValAddr :: TACAddr -> Type -> Type -> TACAddr
maybeConvertValAddr a ty1 ty2 | ty1 == ty2 = a
maybeConvertValAddr a ty1 ty2 = convertValAddr a ty1 ty2

convertValAddr :: TACAddr -> Type -> Type -> TACAddr
convertValAddr addr@(ValAddr a) tyFrom tyTo = case (tyFrom, tyTo) of
  (BasicType TypeInt, BasicType TypeReal) -> ValAddr $ show $ getRealValue addr
  (BasicType TypeChar, BasicType TypeInt) -> ValAddr $ show $ ord $ good
          where good = read (Data.ByteString.Char8.unpack (Data.ByteString.Char8.pack a)) :: Char
  (BasicType TypeChar, BasicType TypeReal) -> convertValAddr addr1 int_ real_
    where addr1 = convertValAddr addr char_ int_
  otherwise -> NoAddr
convertValAddr _ _ _ = NoAddr

--------------------------------------------------------------------
-- Operations on values: precompute constant vals.

intBOp :: TACOp -> TACAddr -> TACAddr -> TACAddr
intBOp op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          "add" -> show ((getIntValue x) + (getIntValue y))
          "mul" -> show ((getIntValue x) * (getIntValue y))
          "sub" -> show ((getIntValue x) - (getIntValue y))
          "mod" -> show (mod (getIntValue x) (getIntValue y))
          "pow" -> show ((getIntValue x) ^ (getIntValue y))
intBOp _ _ _ = NoAddr

intUOp :: TACOp -> TACAddr -> TACAddr
intUOp op x@(ValAddr xv) = ValAddr res
  where res = case op of
          "+" -> xv
          "negate" -> show (- (getIntValue x))
intUOp _ _ = NoAddr

------------

realBOp :: TACOp -> TACAddr -> TACAddr -> TACAddr
realBOp op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          "add" -> show ((getRealValue x) + (getRealValue y))
          "mul" -> show ((getRealValue x) * (getRealValue y))
          "sub" -> show ((getRealValue x) - (getRealValue y))
          "div" -> show ((getRealValue x) / (getRealValue y))
realBOp _ _ _ = NoAddr

realUOp :: TACOp -> TACAddr -> TACAddr
realUOp op x@(ValAddr xv) = ValAddr res
  where res = case op of
          "+" -> xv
          "negate" -> show (- (getRealValue x))
realUOp _ _ = NoAddr

------------

boolBOp :: TACOp -> TACAddr -> TACAddr -> TACAddr
boolBOp op x@(ValAddr bx) y@(ValAddr by) = ValAddr res
  where res = case op of
          "&&"| bx == "False" -> bx
              | otherwise     -> by
          "||"| bx == "True"  -> bx
              | otherwise     -> by
boolBOp _ _ _ = NoAddr

boolUOp :: TACOp -> TACAddr -> TACAddr
boolUOp op x@(ValAddr bx) = ValAddr res
  where res = case op of
          "!"| bx == "False" -> "True"
             | otherwise     -> "False"
boolUOp _ _ = NoAddr

------------

boolIntRel :: Rel -> TACAddr -> TACAddr -> TACAddr
boolIntRel op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          RelLess _ -> show ((getIntValue x) < (getIntValue y))
          RelLessEq _ -> show ((getIntValue x) <= (getIntValue y))
          RelGreater _ -> show ((getIntValue x) > (getIntValue y))
          RelGreaterEq _ -> show ((getIntValue x) >= (getIntValue y))
          RelNotEq _ -> show ((getIntValue x) /= (getIntValue y))
          RelEq _ -> show ((getIntValue x) == (getIntValue y))
boolIntRel _ _ _ = NoAddr

boolRealRel :: Rel -> TACAddr -> TACAddr -> TACAddr
boolRealRel op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          RelLess _ -> show ((getRealValue x) < (getRealValue y))
          RelLessEq _ -> show ((getRealValue x) <= (getRealValue y))
          RelGreater _ -> show ((getRealValue x) > (getRealValue y))
          RelGreaterEq _ -> show ((getRealValue x) >= (getRealValue y))
          RelNotEq _ -> show ((getRealValue x) /= (getRealValue y))
          RelEq _ -> show ((getRealValue x) == (getRealValue y))
boolRealRel _ _ _ = NoAddr

boolCharRel :: Rel -> TACAddr -> TACAddr -> TACAddr
boolCharRel op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          RelLess _ -> show ((getCharValue x) < (getCharValue y))
          RelLessEq _ -> show ((getCharValue x) <= (getCharValue y))
          RelGreater _ -> show ((getCharValue x) > (getCharValue y))
          RelGreaterEq _ -> show ((getCharValue x) >= (getCharValue y))
          RelNotEq _ -> show ((getCharValue x) /= (getCharValue y))
          RelEq _ -> show ((getCharValue x) == (getCharValue y))
boolCharRel _ _ _ = NoAddr

boolStringRel :: Rel -> TACAddr -> TACAddr -> TACAddr
boolStringRel op (ValAddr x) (ValAddr y) = ValAddr res
  where res = case op of
          RelNotEq _ -> show (x /= y)
          RelEq _ -> show (x == y)
boolStringRel _ _ _ = NoAddr


boolRel :: Rel -> TACAddr -> TACAddr -> TACAddr
boolRel op (ValAddr x) (ValAddr y) = ValAddr res
  where res = case op of
          RelNotEq _ -> show (x /= y)
          RelEq _ -> show (x == y)
boolRel _ _ _ = NoAddr

-------------------

dispatchBOp :: Type -> TACOp -> TACAddr -> TACAddr -> TACAddr
dispatchBOp ty op eVal1 eVal2 = case ty of
  BasicType TypeInt -> intBOp op eVal1 eVal2
  BasicType TypeReal -> realBOp op eVal1 eVal2
  BasicType TypeChar -> intBOp op (convertValAddr eVal1 ty int_) (convertValAddr eVal2 ty int_)

dispatchUOp :: Type -> TACOp -> TACAddr -> TACAddr
dispatchUOp ty op eVal1 = case ty of
  BasicType TypeInt -> intUOp op eVal1
  BasicType TypeReal -> realUOp op eVal1
  BasicType TypeChar -> intUOp op (convertValAddr eVal1 ty int_)

dispatchBoolRel :: Type -> Rel -> TACAddr -> TACAddr -> TACAddr
dispatchBoolRel ty rel eVal1 eVal2 = case ty of
  BasicType TypeInt -> boolIntRel rel eVal1 eVal2
  BasicType TypeReal -> boolRealRel rel eVal1 eVal2
  BasicType TypeChar -> boolCharRel rel eVal1 eVal2
  BasicType TypeBool -> boolRel rel eVal1 eVal2
  BasicType TypeString -> boolStringRel rel eVal1 eVal2



--------------------------------------------------------------------
---------------------------- NAMES ---------------------------------

-- Extract the BIdent of a *simple* left expression.
getBId :: LeftExpr -> BIdent
getBId lexpr = case lexpr of
  LEId bIdent ->  bIdent
  LEDeref lexpr1 -> getBId lexpr1
  LEArray lexpr1 _ -> getBId lexpr1
  LEBra lexpr1 -> getBId lexpr1
  LEPreIncrem lexpr1 -> getBId lexpr1
  LEPreDecr lexpr1 -> getBId lexpr1


-- Extract the name of a *simple* left expression.
getName :: LeftExpr -> Id
getName lexpr = case lexpr of
  LEId (BIdent ((_,_), id)) -> id
  LEDeref lexpr1 -> getName lexpr1
  LEArray lexpr1 _ -> getName lexpr1
  LEBra lexpr1 -> getName lexpr1
  LEPreIncrem lexpr1 -> getName lexpr1
  LEPreDecr lexpr1 -> getName lexpr1

getId :: BIdent -> Id
getId (BIdent ((_,_), id)) = id

getIdAddress :: BIdent -> TACAddr
getIdAddress (BIdent ((l,c), id)) = Addr (id++('@':(show l))++(',':(show c)))

getIdLabel :: BIdent -> TACLabel
getIdLabel (BIdent ((l,c), id)) = Label (id++('@':(show l))++(',':(show c)))

getAddressName :: TACAddr -> Id
getAddressName (Addr name) = name


--------------------------------------------------------------------
--------------------------- POSITIONS ------------------------------

relPos :: Rel -> Pos
relPos rel = case rel of
  RelLess (BLe (pos,_))         -> pos
  RelLessEq (BLeEq (pos,_))     -> pos
  RelGreater (BGr (pos,_))      -> pos
  RelGreaterEq (BGrEq (pos,_))  -> pos
  RelNotEq (BNotEq (pos,_))     -> pos
  RelEq (BEq (pos,_))           -> pos
  
firstStmtPos :: LoopCmd -> (Pos,String)
firstStmtPos stmt =
    case stmt of 
        LoopExitOn (BExitOn (pos,s)) e   -> (pos,s)
        LoopExit (BExit (pos,s))         -> (pos,s)
        LoopContinue (BContinue (pos,s)) -> (pos,s)

firstExprPos :: Expr -> Pos
firstExprPos ev =
    case ev of
        ExprVal v ->
            case v of
                IntVal (BInteger (pos,_))  -> pos
                RealVal (BReal (pos,_))    -> pos
                CharVal (BChar(pos,_))     -> pos
                BoolVal (BBool(pos,_))     -> pos
                StringVal (BString(pos,_)) -> pos
        ExprLeft v ->
            case v of
                LEId id@(BIdent(pos,_))    -> pos
                LEDeref le                 -> firstExprPos (ExprLeft le)
                LEArray le _               -> firstExprPos (ExprLeft le)
                LEBra le                   -> firstExprPos (ExprLeft le)
                LEPreIncrem le             -> firstExprPos (ExprLeft le)
                LEPreDecr le               -> firstExprPos (ExprLeft le)
        ExprArray _ (e:_) -> firstExprPos e
        ExprIf e _ _ -> firstExprPos e
        ExprArray (BLBra (pos,_)) _ -> pos
        ExprCreate _ _ _ e -> firstExprPos e
        ExprAddress le -> firstExprPos (ExprLeft le)
        ExprCall id@(BIdent(pos,_)) _ -> pos
        ExprBrack e -> firstExprPos e
        ExprPostIncr le -> firstExprPos (ExprLeft le)
        ExprPostDecr le -> firstExprPos (ExprLeft le)
        ExprPow e _ _  -> firstExprPos e
        ExprMul e _ _-> firstExprPos e
        ExprMod e _ _-> firstExprPos e
        ExprDiv e _ _-> firstExprPos e
        ExprSum e _ _-> firstExprPos e
        ExprMinus e _ _-> firstExprPos e
        ExprUnaryMinus _ e -> firstExprPos e
        ExprUnaryPlus _ e -> firstExprPos e
        ExprAnd e _ _ -> firstExprPos e
        ExprOr e _ _ -> firstExprPos e
        ExprNot _ e -> firstExprPos e
        ExprRel e _ _  -> firstExprPos e


--------------------------------------------------------------------
----------------------------- PRINTS --------------------------------

showFunction :: String -> [Type] -> String
showFunction out [] = (init out) ++ ")"
showFunction out (t:types) =
    showFunction (out++(showType t )++",") types

showType :: Type -> String
showType ty = case ty of
  BasicType TypeInt -> "int"
  BasicType TypeBool -> "bool"
  BasicType TypeChar -> "char"
  BasicType TypeReal -> "real"
  BasicType TypeString -> "string"
  ErrorType -> "error"
  VoidType -> "void"
  PointerType t -> ('°':(showType t))
  ArrayType _ t _ ->  "Array <" ++ (showType t) ++ ">"
  ArrayDimType _ t _ _ ->  "Array <" ++ (showType t) ++ ">"
  Array t 0 -> "Array <" ++ (showType t) ++ ">(" ++ "?" ++")"
  Array t n -> "Array <" ++ (showType t) ++ ">(" ++ (show n) ++")"

showAssignType :: Type -> String
showAssignType ty = case ty of
  BasicType TypeInt -> "int"
  BasicType TypeBool -> "bool"
  BasicType TypeChar -> "char"
  BasicType TypeReal -> "real"
  BasicType TypeString -> "addr"
  ErrorType -> error $ "showing ErrorType"
  VoidType -> error $ "showing VoidType"
  otherwise -> "addr"

showTACRel :: Rel -> String
showTACRel rel = case rel of
  RelLess _ -> "less"
  RelLessEq _-> "less_eq"
  RelGreater _ -> "gr"
  RelGreaterEq _ -> "gr_eq"
  RelNotEq _-> "not_eq"
  RelEq _-> "equal"

showRel :: Rel -> String
showRel rel = case rel of
  RelLess _ -> "<"
  RelLessEq _-> "<="
  RelGreater _ -> ">"
  RelGreaterEq _ -> ">="
  RelNotEq _-> "<>"
  RelEq _-> "="

builtIn :: String -> TACLabel
builtIn name = Label ("built_in_"++name)


--------------------------------------------------------------------
--------------------------- GENERAL --------------------------------

trust :: Maybe a -> a
trust (Just x) = x

isFuncDecl :: Stmt -> Bool
isFuncDecl (StmtDecl (FuncDef _ _ _ _ _ _)) = True
isFuncDecl (StmtDecl (ProcDef _ _ _ _)) = True
isFuncDecl _ = False


notRel :: Rel -> Rel
notRel rel = case rel of
  RelLess (BLe (pos,_))         -> RelGreaterEq (BGrEq(pos,">="))
  RelLessEq (BLeEq (pos,_))     -> RelGreater (BGr (pos,">"))
  RelGreater (BGr (pos,_))      -> RelLessEq (BLeEq (pos,"<="))
  RelGreaterEq (BGrEq (pos,_))  -> RelLess (BLe (pos,"<"))
  RelNotEq (BNotEq (pos,_))     -> RelEq (BEq (pos,"=")) 
  RelEq (BEq (pos,_))           -> RelNotEq (BNotEq (pos,"<>"))

-------------------------------------------------------------------
-- Private aliases:

int_ = (BasicType TypeInt)
real_ = (BasicType TypeReal)
char_ = (BasicType TypeChar)
bool_ = (BasicType TypeBool)
string_ = (BasicType TypeString)
void_ = VoidType
addr_ = (BasicType TypeString) -- only for prints.

negExpr_ cond = ExprNot not_ cond
exitOnStmt_ cond = StmtLoopCmd $ LoopExitOn exiton_ cond
preIncr_ bId = ExprLeft $ LEPreIncrem $ LEId bId
preDecr_ bId = ExprLeft $ LEPreDecr $ LEId bId
relGEq_ = RelGreaterEq greateq_
relLEq_ = RelLessEq lesseq_
relLess_ = RelLess less_
zeroExpr_ = ExprVal $ IntVal $ BInteger ((0,0), "0")

eq_ = BEq ((0,0),"=")
neq_ = BNotEq ((0,0),"<>")
plus_ = BPlus ((0,0),"+")
minus_ = BMinus ((0,0),"-")
bra_ = BLBra ((0,0), "[")
lesseq_ = BLeEq ((0,0),"<=")
less_ = BLe ((0,0),"<")
greateq_ = BGrEq ((0,0),">=")
exiton_  = BExitOn ((0,0), "exiton")
not_ = BNot ((0,0),"not")

