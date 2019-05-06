module StaticAnalysis where

import AbsRnb
import PrettyPrinter
import ErrorStaticAnalysis
import Utils
import Data.Char (ord, readLitChar)
import Data.Map (Map, member,lookup,toList)
import Data.ByteString.Char8 (unpack, ByteString, pack) 
import qualified Data.Map as Map

-------------------------------------------------
----------------- DATA & TYPE -------------------
-------------------------------------------------

type CTime = Bool
type Names = Map Id EnvRecord

data EnvRecord =
    VarInfo { 
        typ :: Type,                -- Variable Type.
        cTime :: CTime,             -- True if is const at CompileTime
        pos :: Pos,                 -- SourceLocation <line, column>
        init :: Maybe Int,          -- If the value of a constant is known at CompileTime
        const :: Bool
        } | 
    FuncInfo {
        retType :: Type,            -- Function return Type
        params :: [Param],          -- List of parameters
        pos :: Pos                  -- SourceLocation <line,column>
    } deriving (Eq, Show)

data Env =  Env {
  names :: Names,                   -- Map with all the name declared
  parent :: Maybe Env,              -- Parent block
  errors :: [(String, Pos)],        -- String of errors
  loop   :: Bool,                   -- True if the block is a loopBlock
  funcCheck :: (Maybe Names,        -- Names in the functions declarations
                Maybe BIdent,       -- Name of the return variable
                Bool,               -- True if all the scope at the level n-1 have an initialization of the variable
                Bool,               -- True if one of the scope at the level n or n-1 initialize the variable
                Bool,               -- False if there is a continue/return/exit or exiton
                Bool),              -- True if there is a Default case.
  loopFor :: Bool                   -- True if a for stmt is found.           
} deriving (Eq, Show)

data ExprRes = ExprRes{             --ExprRes incapsulate the result of the comutation of an Expression
    eTyp :: Type,                   --Result Type
    eCTime :: CTime,                --True if the result is known at compile time
    eVal :: Maybe Int,              --The integer value that the Static Analysis module can calculate at compile time
    eConst ::  Bool                 --If the right expression is Constant
} deriving (Eq, Show)


-------------------------------------------------
----------------- DEFAULT VALUES ----------------
-------------------------------------------------

exprError :: ExprRes
exprError = ExprRes ErrorType False Nothing False

defaultFuncList :: [(Id, EnvRecord)]
defaultFuncList =  [("writeInt", (FuncInfo VoidType [ Parameter (BIdent((0,0),"valInt")) ModVal (BasicType TypeInt)] (0,0))),
                   ("writeReal", (FuncInfo VoidType [ Parameter (BIdent((0,0),"valReal")) ModVal (BasicType TypeReal)] (0,0))),
                   ("writeChar", (FuncInfo VoidType [ Parameter (BIdent((0,0),"valChar")) ModVal (BasicType TypeChar)] (0,0))),
                   ("writeString", (FuncInfo VoidType [ Parameter (BIdent((0,0),"valString")) ModVal (BasicType TypeString)] (0,0))),
                   ("readInt", (FuncInfo (BasicType TypeInt) [] (0,0))),
                   ("readReal", (FuncInfo (BasicType TypeReal) [] (0,0))),
                   ("readChar", (FuncInfo (BasicType TypeChar) [] (0,0))),
                   ("readString", (FuncInfo (BasicType TypeString) [] (0,0)))]

defaultEnv :: Env
defaultEnv = Env
             (Map.fromList defaultFuncList)               -- Map (Key Id, Value EnvRecord) 
             Nothing                                      -- Previous
             []                                           -- List of Errors
             False                                        -- Not a loop block
             (Nothing, Nothing, False, True, True, False) -- FuncCheck 
             False                                        -- Not in a loop stmt

-------------------------------------------------
-------- MAIN CATEGORIES HANDLER ----------------
-------------------------------------------------
--Program entry point. Called by Compiler.hs. In the env structure there are all the possible errors.
rnbStaticAnalysis :: Program -> Env
rnbStaticAnalysis (Start decls) = saDecls (addFuncsFromDecls defaultEnv decls) decls

saDecls :: Env -> [Decl] -> Env
saDecls env [] = checkMain env
saDecls env (d:ds) = saDecls ( saDecl env d) ds

saDecl :: Env -> Decl -> Env
saDecl env decl = 
    case decl of
        VarDecl  id ty        -> handlerVarDecl env id ty
        VarDef   id ty (BAss (posA,_)) ex     -> handlerVarDef env id ty ex posA 
        ConstDef id ty (BAss (posA,_)) ex     -> handlerConstVarDef env id ty ex posA
        FuncDef funcId params outId outType stmts closeFuncId ->copyErrors env sonEnv
            where sonEnv = handlerFuncDef (addFuncsFromStmts (createEnv $ env)  stmts)
                                             funcId params outId outType stmts closeFuncId
        ProcDef procId params stmts closeProcId -> copyErrors env sonEnv
            where sonEnv  = handlerProcDef (addFuncsFromStmts (createEnv env) stmts) 
                            procId params stmts closeProcId

saExpr :: Env -> Expr -> (Env, ExprRes)
saExpr env expr = 
    case expr of
        ExprVal val                        -> handlerExprVal env val
        ExprCreate _ ty _ expr             -> handlerExprCreate env expr ty
        ExprLeft le                        -> handlerLeftExpr env le
        ExprAddress le                     -> handlerExprAddress env le
        ExprCall id exprs                  -> handlerExprCall env id exprs 
        ExprBrack expr                     -> saExpr env expr
        ExprUnaryMinus _ expr              -> handlerUnaryExpr env expr "-"
        ExprUnaryPlus _ expr               -> handlerUnaryExpr env expr "+"
        ExprPostIncr le                    -> handlerUnaryLeftExpr env le
        ExprPostDecr le                    -> handlerUnaryLeftExpr env le
        ExprPow ex1 (BUpTo (pos,s)) ex2    -> handlerBinaryExpr env ex1 ex2 (pos,s)
        ExprMul ex1 (BMul (pos,s)) ex2     -> handlerBinaryExpr env ex1 ex2 (pos,s)
        ExprMod ex1 (BMod (pos,s)) ex2     -> handlerExprDiv env ex1 ex2 (pos,s)
        ExprDiv ex1 (BDiv (pos,s)) ex2     -> handlerExprDiv env ex1 ex2 (pos,s)
        ExprSum ex1 (BPlus (pos,s)) ex2    -> handlerBinaryExpr env ex1 ex2 (pos,s)
        ExprMinus ex1 (BMinus (pos,s)) ex2 -> handlerBinaryExpr env ex1 ex2 (pos,s)
        ExprAnd ex1 (BAnd (pos,s)) ex2     -> handlerBinaryBool env ex1 ex2 (pos,s)
        ExprOr ex1 (BOr (pos,s)) ex2       -> handlerBinaryBool env ex1 ex2 (pos,s)
        ExprNot (BNot (pos,s)) ex          -> handlerUnaryBool env ex (pos,s) 
        ExprRel ex1 rel ex2                -> handlerRelExpr env ex1 ex2 rel
        ExprIf expr1 exprBool expr2              -> handlerExprIf env expr1 exprBool expr2
        ExprArray (BLBra (pos,_)) (e:expr) -> handlerExprArray env (e:expr) inductiveType pos (length (e:expr),len) 
            where (inductiveEnv, eRes@(ExprRes inductiveType cTimeE valE constE)) = saExpr env e
                  len = getExprLen env e
        ExprArray (BLBra (pos,_)) []       -> handlerEmptyList env pos

saStmts :: Env -> [Stmt] -> Env
saStmts env [] = env
saStmts env (s@(StmtReturn (BReturn (pos, _))):ss:sss) =saStmts (saStmt (addError env  (DeathCode pos ,pos))s ) (ss:sss) 
saStmts env (s:stmts) = saStmts (saStmt env s) stmts

saStmt ::Env -> Stmt ->Env
saStmt env s = 
    case s of
        StmtAssign le (BAss (posA,_)) expr -> handlerStmtAssign env (ExprLeft le) expr posA
        StmtDecl decl -> saDecl env decl
        StmtExpr expr -> handlerStmtExpr env expr
        StmtLoop (BLoop (pos,_)) stmts _-> copyEnvInfo env sonEnv Nothing
            where sonEnv  = checkDetLoop (saStmts (addFuncsFromStmts ((createEnv env) {loop = True})  stmts) stmts) pos
        StmtLoopCmd loopStmts -> handlerStmtLoopCmd env loopStmts
        StmtReturn (BReturn (pos,_))-> handlerStmtReturn env {funcCheck = (name, id, retBool, boolFather, False, def)} pos
            where (name, id, retBool, boolFather,_, def) = funcCheck env
        StmtLocal stmts -> copyEnvInfo env sonEnv Nothing
            where sonEnv  = saStmts (addFuncsFromStmts (createEnv env)  stmts) stmts
        StmtIf expr stmts elseStmts -> copyEnvInfo env sonEnv (Just sonEnv2)
            where sonEnv  = saStmts (addFuncsFromStmts (createEnv env)  stmts) stmts
                  sonEnv2 = handlerElseStmts (checkBooleanCondition (createEnv env) expr) elseStmts
        StmtCase (BCase (pos,_)) expr cases _ -> handlerStmtCase nEnv exprType cases 0 pos
            where (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr env expr
                  nEnv = checkCaseExpr exprEnv exprType expr
        StmtFor sId sTy (BAss (pos,s)) sExpr dir lExpr stmts -> copyEnvInfo env sonEnv Nothing
            where sonEnv  = saStmts (addFuncsFromStmts  (handlerStmtFor (createEnv env){loop = False, loopFor = True} sId (BasicType sTy) pos sExpr lExpr stmts dir) stmts) stmts
        StmtTryCatch _ stmtsTry _ stmtsCatch _ -> copyEnvInfo env sonEnv (Just sonEnv2)
            where sonEnv  = saStmts (addFuncsFromStmts (createEnv env)  stmtsTry) stmtsTry
                  sonEnv2 = saStmts (addFuncsFromStmts (createEnv env)  stmtsCatch) stmtsCatch

-------------------------------------------------
---------------- DECLARATIONS -------------------
-------------------------------------------------

handlerVarDecl:: Env -> BIdent -> Type -> Env
handlerVarDecl env id@(BIdent (pos,_)) (PointerType _) =
  addError redeclEnv (ErrorDeclaration id "Pointer",pos)
  where (redeclEnv, redecl)      = checkEnvRedecl env id 1 
handlerVarDecl env id@(BIdent (pos,_)) (ArrayType _ _ _) =
  addError redeclEnv (ErrorDeclaration id "Array",pos)
  where (redeclEnv, redecl)      = checkEnvRedecl env id 1 
handlerVarDecl env id@(BIdent (pos,_)) (ArrayDimType _ _ _ _) =
  addError redeclEnv (ErrorDeclaration id "Array",pos)
  where (redeclEnv, redecl)      = checkEnvRedecl env id 1
handlerVarDecl env id@(BIdent (pos,name)) ty 
    | redecl = checkedEnv
    | otherwise = addSymbol checkedEnv name (VarInfo ty False pos Nothing False)
    where (checkedEnv, redecl) = checkEnvRedecl env id 1

handlerVarDef :: Env -> BIdent -> Type -> Expr -> Pos -> Env
handlerVarDef env id@(BIdent (pos,name)) ty expr posA 
    | redecl    = addressEnv
    | otherwise = addSymbol addressEnv name (VarInfo arrType False pos Nothing False)
    where (redeclEnv, redecl)      = checkEnvRedecl env id 1 
          (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr redeclEnv expr
          (arrEnv, tmpType, errorFounds) = convertToRedArray exprEnv  ty
          arrType                  = checkArrays tmpType exprType
          (compEnv, comp)          = checkComp exprEnv exprType arrType posA
          envCompArr | errorFounds = arrEnv
                     | otherwise   = compEnv
          (addressEnv,_)           = checkAddressOfConst envCompArr constE expr pos 1

handlerConstVarDef :: Env -> BIdent -> Type -> Expr -> Pos -> Env
handlerConstVarDef env id@(BIdent (pos,name)) ty expr posA
    | redecl    = addressEnv
    | otherwise = addSymbol addressEnv name (VarInfo arrType cTime pos valE True)
    where (redeclEnv, redecl)      = checkEnvRedecl env id 1 
          (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr redeclEnv expr
          (arrEnv, tmpType, errorFounds)= convertToRedArray exprEnv ty
          arrType                  = checkArrays tmpType exprType
          (compEnv, comp)          = checkComp arrEnv exprType arrType posA
          envCompArr | errorFounds = arrEnv
                     | otherwise   = compEnv
          (addressEnv,_)           = checkAddressOfConst envCompArr constE expr pos 0

handlerFuncDef:: Env ->BIdent -> [Param] -> BIdent -> Type -> [Stmt] -> BIdent -> Env
handlerFuncDef env funcId@(BIdent (pos,_)) params 
               outId@(BIdent (posOut,_)) outType stmts closeFuncId = envReturnChecked
    where envNameCheck     = checkFunctionNames env funcId closeFuncId
          envParamCheck    = checkFunctionParameters envNameCheck params
          envReturnVar     = addIfNotDeclared envParamCheck outId 1 (VarInfo retTy False posOut Nothing False)
          (envArray, retTy)= case outType of
                                  BasicType _  -> (envReturnVar, outType)
                                  otherwise -> (addError envReturnVar (NonBasicType posOut outType, posOut), ErrorType)
          tmpEnv           = returnInit envArray outId
          envStmts         = saStmts tmpEnv stmts
          envReturnChecked = checkIfReturnVariableIsInitialized envStmts


handlerProcDef:: Env ->BIdent -> [Param] -> [Stmt] -> BIdent -> Env
handlerProcDef env procId params  stmts closeProcId = envStmts
    where envNameCheck    = checkFunctionNames env procId closeProcId
          envParamCheck   = checkFunctionParameters envNameCheck params
          envStmts        = saStmts envParamCheck stmts
 
-------------------------------------------------
---------------- EXPRESSIONS --------------------
-------------------------------------------------

handlerExprCall:: Env -> BIdent -> [Expr] -> (Env, ExprRes)
handlerExprCall env id@(BIdent (pos,_)) exprs
    |isDecl = checkExprsAndParams env exprs par id fpos retTy
    |otherwise = (addError env (NotDeclared id,pos), exprError)
    where maybeDecl = isDeclVar env id
          (isDecl, par, retTy,fpos) = case maybeDecl of 
                            Just (FuncInfo ty params poss ) -> (True, params, ty,poss)
                            Just (VarInfo _ _ _ _ _) -> (False, [], VoidType, pos)
                            Nothing -> (False,[], VoidType,pos)

handlerUnaryBool :: Env -> Expr -> (Pos, String) ->(Env, ExprRes)
handlerUnaryBool env ex (pos, op) 
    |isBool = (envE1, (ExprRes (BasicType TypeBool) False Nothing False))
    |isError typeE1 = (envE1, exprError)
    |otherwise = (addError envE1 (BoolNotError pos typeE1 op, pos), exprError)
    where (envE1, eRes1@(ExprRes typeE1 cTimeE1 valE1 constE1)) = saExpr env ex
          isBool = typeE1 == (BasicType TypeBool)

handlerBinaryBool:: Env -> Expr -> Expr -> (Pos,String) -> (Env, ExprRes)
handlerBinaryBool env ex1 ex2 (pos, op) 
    |areBool = (envE2, (ExprRes (BasicType TypeBool) False Nothing False))
    |areErrors typeE1 typeE2 = (envE2, exprError)
    |otherwise = (addError envE2 (BoolOpError pos typeE1 typeE2 op, pos), exprError)
    where (envE1, eRes1@(ExprRes typeE1 cTimeE1 valE1 constE1))    = saExpr env ex1
          (envE2, eRes2@(ExprRes typeE2 cTimeE2 valE2 constE2))    = saExpr envE1 ex2
          areBool = typeE1 == (BasicType TypeBool) && typeE2 == typeE1

handlerRelExpr :: Env -> Expr -> Expr -> Rel -> (Env, ExprRes)
handlerRelExpr env ex1 ex2 rel 
    | comparable || applyRel = (envE2, (ExprRes (BasicType TypeBool) False Nothing False))
    | areErrors typeE1 typeE2 = (envE2, exprError)
    | otherwise = (addError envE2 (RelError pos typeE1 typeE2 rel, pos), exprError)
    where (envE1, eRe1@(ExprRes typeE1 cTimeE1 valE1 constE1))    = saExpr env ex1
          (envE2, eRe2@(ExprRes typeE2 cTimeE2 valE2 constE2))    = saExpr envE1 ex2
          comparable = comparableTypes typeE1 typeE2
          equalityCSTR = typeE1 == (BasicType TypeString) && typeE2 == (BasicType TypeChar) ||
                         typeE2 == (BasicType TypeString) && typeE1 == (BasicType TypeChar)
          pos        = relPos rel
          applyRel   = (equalityCSTR ||comparable || 
                     (typeE1 == typeE2 && equalitySupport typeE1))
                     && case rel of
                            (RelEq _) -> True
                            (RelNotEq _) -> True
                            otherwise -> False

handlerExprDiv :: Env -> Expr -> Expr -> (Pos,String)  -> (Env, ExprRes)
handlerExprDiv env ex1 ex2 op
    |isZero = (addError env (DivideByZero pos,pos), exprError)
    |otherwise = handlerBinaryExpr env ex1 ex2 op
    where (_, eRes@(ExprRes _ _ valE _)) = saExpr env ex2
          pos = firstExprPos ex2
          isZero = case valE of
                        Just 0 -> True
                        otherise -> False

handlerBinaryExpr :: Env -> Expr -> Expr ->(Pos,String) -> (Env, ExprRes)
handlerBinaryExpr env ex1 ex2 (opPos, op)
    | numericalComp e1Type && numericalComp e2Type = (resEnv,res )
    | areErrors e1Type e2Type = (e2Env, exprError)
    | otherwise = (addError e2Env (ArithmeticError e1Type e2Type opPos op , opPos), exprError) 
    where (e1Env, eRes1@(ExprRes e1Type e1CTime valE1 constE1)) = saExpr env ex1
          (e2Env, eRes2@(ExprRes e2Type e2CTime valE2 constE2)) = saExpr e1Env ex2
          retTy  = operationReturnType e1Type e2Type op
          resEnv = checkArithmeticTypes e2Env e1Type e2Type retTy (opPos, op) 
          value  = maybeOp op valE1 valE2
          res    = ExprRes retTy (e1CTime && e2CTime) value False

handlerUnaryExpr :: Env -> Expr -> String -> (Env, ExprRes)
handlerUnaryExpr env expr op=
    case eType of
        ErrorType -> (eEnv, eRes)
        PointerType _ -> (addError eEnv (PointerArithmetic pos, pos), exprError)
        Array _ _->   (addError eEnv (PointerArithmetic pos, pos), exprError)
        otherwise | leqType eType (BasicType TypeInt) ||eType == (BasicType TypeReal)  -> (eEnv, eRes{eVal = signVal, eTyp=newType  })
                  | True ->(addError eEnv (NonNumericValue pos, pos),exprError)
    where (eEnv,eRes@(ExprRes eType eCTime valE constE)) = saExpr env expr
          newType = getMaxType eType (BasicType TypeInt)
          pos = firstExprPos expr
          signVal | op=="-"   = maybeNegative valE
                  | otherwise = valE


  

handlerExprAddress :: Env -> LeftExpr -> (Env, ExprRes)
handlerExprAddress env le =
    case eType of
        ErrorType -> (eEnv, exprError)
        otherwise -> (eEnv,  (ExprRes (PointerType eType) eCTimeE Nothing constE))
        where (eEnv, eRes@(ExprRes eType eCTimeE valE constE)) = handlerLeftExpr env le
              id@(BIdent (pos,name)) = getBId le

handlerExprVal:: Env -> Val -> (Env, ExprRes)
handlerExprVal env val =
    case val of 
        IntVal (BInteger(_,val)) -> (env,(ExprRes (BasicType TypeInt) True (Just (read val)) False))
        RealVal   _ -> (env,( ExprRes (BasicType TypeReal)   True Nothing False))
        CharVal (BChar(_,val)) -> (env,( ExprRes (BasicType TypeChar) True (Just (ord $ good) )False))
          where good = read (Data.ByteString.Char8.unpack (Data.ByteString.Char8.pack val)) :: Char  -- 
        BoolVal   _ -> (env,( ExprRes (BasicType TypeBool)   False Nothing False))
        StringVal _ -> (env,( ExprRes (BasicType TypeString) False Nothing False))

handlerExprArray :: Env -> [Expr] -> Type -> Pos -> (Int,Int) -> (Env, ExprRes)
handlerExprArray env [] typ _ (lenExpr, _)
    | typ == ErrorType = (env, (ExprRes (Array ErrorType lenExpr) False Nothing False))
    | otherwise = (env, (ExprRes (Array typ lenExpr) True Nothing False))

handlerExprArray env (e:expr) typ pos (lenExpr, lenSubExpr)
    |isAddressOfConst = handlerExprArray envAddress expr ErrorType pos (lenExpr,(-1))
    |eLen /= lenSubExpr  && lenSubExpr /=(-1)= handlerExprArray envNonLen expr ErrorType pos (lenExpr,(-1))
    |typ == exprType || isError typ = handlerExprArray exprEnv expr typ pos (lenExpr,lenSubExpr)
    |otherwise = handlerExprArray envNonOmo expr ErrorType pos (lenExpr,(-1))
    where (exprEnv, eRes@(ExprRes exprType cTimeE valE constE)) = saExpr env e
          envNonOmo = addError exprEnv (NonOmogeneusArray pos, pos)
          envNonLen = addError exprEnv (DifferentLenght pos, pos)
          (envAddress,isAddressOfConst)= checkAddressOfConst exprEnv constE e (firstExprPos e) 0
          eLen = getExprLen env e

handlerExprCreate:: Env -> Expr -> Type -> (Env, ExprRes)  
handlerExprCreate env expr ty
    | isBT = (addError envC (NonBasicType pos ty, pos), exprError)
    | isCTime  = (envC,(ExprRes (Array ty (trust maybeInt)) False maybeInt False))
    | otherwise = (envC, exprError)
    where (envC, isCTime, maybeInt) = checkConstExpr env expr
          pos = firstExprPos expr
          isBT = case ty of (BasicType _) -> False
                            otherwise -> True

handlerLeftExpr:: Env -> LeftExpr -> (Env, ExprRes)
handlerLeftExpr env le =
    case le of 
        LEId id -> handlerIdent env id
        LEDeref le -> handlerDeref env le
        LEArray le dim -> handlerArray env le dim
        LEBra le -> handlerLeftExpr env le
        LEPreIncrem le -> handlerUnaryLeftExpr env le
        LEPreDecr le-> handlerUnaryLeftExpr env le

handlerDeref :: Env -> LeftExpr -> (Env, ExprRes)
handlerDeref  env le = 
    case nType of 
        (PointerType t) -> (nEnv, (ExprRes t nCTime valE False))
        otherwise       -> (addError nEnv (DereferenceExcess id , pos), exprError)
    where (nEnv, eRes@(ExprRes nType nCTime valE constE))  = handlerLeftExpr env le
          id@(BIdent (pos,name)) = getBId le

handlerArray :: Env -> LeftExpr -> Dim -> (Env, ExprRes)
handlerArray env le (DimArray _ expr) 
    | not isDecl = (addError env (NotDeclared id, pos), exprError)
    | not (leqType typeEx (BasicType TypeInt)) = (addError envExpr (NonIntegerExprInArrayAccess posExpr, posExpr), exprError)
    | leqType typeEx (BasicType TypeInt) && cTimeEx && not gZero  = (addError envExpr (NegativeSizeIndex posExpr, posExpr), exprError)
    | otherwise =case typeE of 
        (Array t _)  ->(envExpr, eRes {eTyp = t})
        otherwise       -> (addError envExpr (NotAnArray pos , pos), exprError)
    where id@(BIdent (pos,name)) = getBId le
          (nEnv, eRes@(ExprRes typeE cTimeE valE constE))  = handlerLeftExpr env le
          posExpr = firstExprPos expr
          gZero = case valEx of
                    Nothing -> True
                    Just x  -> x >= 0
          (envExpr,eExpr@(ExprRes typeEx cTimeEx valEx _) )  = saExpr nEnv expr
          isDecl = case isDeclVar env id of
                        Just _ -> True
                        otherwise -> False


handlerIdent :: Env -> BIdent -> (Env, ExprRes)
handlerIdent env id = case checkDeclIdent env id of 
        (nEnv,Just (VarInfo ty cTime _ x const)) -> (nEnv, (ExprRes ty cTime x const))
        (nEnv,Just (FuncInfo _ _ pos))           -> (addError nEnv (FunctionAsIdentifier pos,pos), exprError)
        (nEnv,_)                                 -> (nEnv , exprError)

handlerUnaryLeftExpr :: Env -> LeftExpr -> (Env, ExprRes)
handlerUnaryLeftExpr env le 
    | constE && typeE /= ErrorType = (addError env (ChangingConstVar id, pos), exprError)
    | otherwise = checkUnaryTypes eEnv typeE eRes pos
      where (eEnv, eRes@(ExprRes typeE cTimeE valE constE)) = handlerLeftExpr env le
            id@(BIdent (pos,name)) = getBId le

handlerEmptyList :: Env -> Pos -> (Env, ExprRes)
handlerEmptyList env pos =
    (addError env (EmptyList pos, pos), exprError)

handlerExprIf :: Env -> Expr -> Expr -> Expr -> (Env, ExprRes)
handlerExprIf env expr1 exprBool expr2 = res
  where env1 = checkBooleanCondition env exprBool
        (env2, (ExprRes tyE1 cTE1 valE1 constE1)) = saExpr env1 expr1
        (env3, (ExprRes tyE2 cTE2 valE2 constE2)) = saExpr env2 expr2
        pos = firstExprPos expr1
        res | tyE1 == tyE2 = (env3, (ExprRes tyE1 False Nothing False))
             | otherwise = (addError env3 (IncompatibleExprs pos tyE1 tyE2, pos), exprError)
    
-------------------------------------------------
------------------ STATEMENTS -------------------
-------------------------------------------------

handlerStmtAssign::Env -> Expr -> Expr ->Pos -> Env
handlerStmtAssign env le@(ExprLeft left) re posA = retEnv
    where (envL, eResL@(ExprRes typeL cTimeL valL constEL)) = saExpr env le
          (envR, eResR@(ExprRes typeR cTimeR valR constER)) = saExpr envL re
          (compEnv, comp)  = checkComp envR typeR typeL posA
          constEnv = checkModifyConstant compEnv id constEL
          (addressEnv,_) = checkAddressOfConst constEnv constER re pos 1
          retEnv = checkReturnInitialized addressEnv id
          id@(BIdent (pos@(l,c),name)) = getBId left

handlerStmtLoopCmd::Env -> LoopCmd -> Env
handlerStmtLoopCmd env loopS
    | loop env  = checkedEnv{funcCheck = (name, id, retBool, boolFather, False, def)}
    | loopFor env = addError checkedEnv{funcCheck = (name, id, retBool, boolFather, False, def)} (ReturnInForLoop pos cmdName,pos)
    | otherwise= addError (env{funcCheck= (name, id, retBool, boolFather, False, def)}) (NotInLoop pos cmdName, pos)
    where (pos,cmdName) = firstStmtPos loopS
          checkedEnv = checkLoopCmd env loopS
          (name, id, retBool, boolFather,_, def) = funcCheck checkedEnv

handlerElseStmts :: Env -> ElseStmt -> Env
handlerElseStmts env elseS = 
    case elseS of
        StmtElseIf expr stmts elsestmts  ->  copyEnvInfo oldEnv sonEnv (Just sonEnv2)
            where oldEnv = checkBooleanCondition env expr
                  sonEnv = saStmts (addFuncsFromStmts (createEnv env)   stmts) stmts
                  sonEnv2 =  handlerElseStmts (createEnv env)  elsestmts
        StmtElse stmts -> copyEnvInfo env sonEnv Nothing
            where sonEnv = saStmts (addFuncsFromStmts (createEnv env)  stmts) stmts
        StmtNoElse -> env

handlerStmtCase :: Env -> Type -> Cases -> Int ->Pos -> Env
handlerStmtCase env baseType ca numBranch pos =
    case ca of
        CaseEps | numBranch == 0 -> addError env (WarEmptyCase pos,pos)
                | otherwise ->  env
        CaseDefault stmts ->  newEnv{funcCheck = (oldN, oldId,  oldSon, oldFather ,break, True)}
            where sonEnv  = saStmts (addFuncsFromStmts (createEnv env)  stmts) stmts
                  newEnv  = (copyEnvInfo env sonEnv Nothing)
                  (oldN, oldId, oldSon, oldFather, break, _) = funcCheck newEnv
        CaseVal expr stmts cases -> handlerStmtCase oldEnv baseType cases (numBranch+1) pos
            where oldEnv  = (copyErrors tmpEnv sonEnv ){funcCheck = (oldN, oldId,  def&&valSon, def&&(valOld || valSon),break, def)} 
                  tmpEnv | not (leqType exprType baseType ) = (addError exprEnv (NonCompExpr baseType exprType nPos,nPos))
                         | otherwise = checkCaseExpr exprEnv exprType expr
                  sonEnv  = saStmts (addFuncsFromStmts (createEnv tmpEnv)  stmts) stmts
                  (oldN,oldId,valOld, _,break, def) = funcCheck tmpEnv
                  (_,_,valSon, _,_,_) = funcCheck sonEnv
                  (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr env expr
                  nPos = firstExprPos expr

handlerStmtExpr::Env->Expr ->Env
handlerStmtExpr env expr =
  case saExpr env expr of
    (exprEnv, (ExprRes ErrorType _ _ _)) -> exprEnv
    (exprEnv, (ExprRes VoidType _ _ _))  -> exprEnv
    (exprEnv, (ExprRes _ _ _ _)) -> 
        case expr of
          ExprPostIncr _                    -> exprEnv
          ExprPostDecr _                    -> exprEnv
          ExprLeft (LEPreDecr _)              -> exprEnv
          ExprLeft  (LEPreIncrem _ )           -> exprEnv
          otherwise -> addError exprEnv (UnsusedValue (pos),pos)
  where pos = firstExprPos expr             


handlerStmtForEach :: Env -> BIdent -> Type -> Expr -> Env
handlerStmtForEach env (BIdent (pos,name)) rTy lExpr =env5
  where (env1,rType,_) = convertToRedArray env rTy
        (env2)         = addSymbol env1 name (VarInfo rType False pos Nothing False)
        (env3, eRes@(ExprRes exprType cTime valE constE)) = saExpr env2 lExpr
        (env4, arrayType) = case exprType of
                    Array lTy _ -> (fst $ checkComp env3 lTy array pos, array)
                      where array = checkArrays rType lTy
                    otherwise   -> (addError env3 (NotAnArrayForEach exprPos ,exprPos),exprType)
        (env5)         = addSymbol env4 name (VarInfo (arrayType) False pos Nothing False)
        exprPos         = firstExprPos lExpr
        
        
        

handlerStmtFor :: Env -> BIdent -> Type -> Pos ->Expr -> Expr -> [Stmt] -> Dir -> Env
handlerStmtFor env sId sTy pos sExpr lExpr stmts dir = env3
  where env1 = handlerConstVarDef env sId sTy sExpr pos
        (exprEnvL,(ExprRes _ cTimeL valL _)) = saExpr env1 sExpr
        (exprEnv, (ExprRes exprType cTime valE constE)) = saExpr exprEnvL lExpr
        exprPos = firstExprPos lExpr
        env2 = checkStmtFor exprEnv cTimeL sTy pos "ex1"
        env3 = checkStmtFor env2 cTime exprType exprPos "ex2"


handlerStmtReturn :: Env -> Pos -> Env
handlerStmtReturn env pos 
  | forL = addError env (ReturnInForLoop pos "return",pos)
  | True = env
  where forL = loopFor env
-------------------------------------------------
------------------- CHECKS ----------------------
-------------------------------------------------

checkIfReturnVariableIsInitialized :: Env -> Env
checkIfReturnVariableIsInitialized env = 
  case val of
    True -> env
    otherwise -> addError env (UnitializedVariable id, pos)
  where (_,Just id@(BIdent (pos@(l,c),name)), _, val,_, _) = funcCheck env

checkArithmeticTypes :: Env -> Type -> Type -> Type -> (Pos, String) -> Env 
checkArithmeticTypes env e1Type e2Type retTy (opPos,op) = 
  case retTy of
        ErrorType  | not (areErrors e1Type e2Type) ->addError env (ArithmeticError e1Type e2Type opPos op , opPos)
                   |otherwise  -> env
        otherwise -> env

checkLoopCmd::Env -> LoopCmd -> Env
checkLoopCmd env (LoopExitOn _ ex) =
    checkBooleanCondition exprEnv ex
    where (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr env ex
checkLoopCmd env _ = env    

checkConstExpr :: Env -> Expr -> (Env,Bool, Maybe Int)
checkConstExpr env expr
    | isError typeExpr = (envExpr, False, Nothing)
    | leqType typeExpr (BasicType TypeInt) && trust value <=0 = (addError env (NegativeSize pos, pos), False, Nothing)
    | leqType typeExpr (BasicType TypeInt) && cTimeExpr = (envExpr, True, value)
    | not (leqType typeExpr (BasicType TypeInt)) = (addError envExpr (NonIntegerExpr pos, pos), False, Nothing)
    | otherwise = (addError envExpr (NonConstantExpr pos, pos), False, Nothing)
    where (envExpr, eRes@(ExprRes typeExpr cTimeExpr value constE)) = saExpr env expr
          pos = firstExprPos  expr

checkEnvRedecl :: Env -> BIdent ->Int -> (Env,Bool)
checkEnvRedecl env id@(BIdent (pos,name)) errorKind
    | isSymbolDeclared env name =
       case errorKind of
          1 -> (addError env (RedeclaredVar id, pos), True)
          2 -> (addError env (RedeclaredFunction id, pos), True)
          3 -> (addError env (RedeclaredParameter id, pos), True)
    | otherwise = (env, False)

checkComp :: Env -> Type -> Type -> Pos -> (Env, Bool)
checkComp env ty1 ty2 pos
    | not (leqType ty1 ty2) =
        (addError env (TypeMismatch pos ty1 ty2, pos), False)
    | otherwise = (env, True)


checkFunctionNames :: Env -> BIdent -> BIdent -> Env
checkFunctionNames env (BIdent (_,nameB)) (BIdent (posE,nameE)) 
    | nameB /= nameE = addError env (FunctionNameMismatch posE nameB, posE) 
    | otherwise      = env

checkFunctionParameters :: Env -> [Param] -> Env
checkFunctionParameters env [] = env
checkFunctionParameters env ((Parameter id@(BIdent (pos,name)) modal ty):params)
    | redeclared = checkFunctionParameters rEnv params
    | otherwise  = checkFunctionParameters (addSymbol rEnv name (VarInfo rType const  pos Nothing const)) params
   where (envRedeclared, redeclared) = checkEnvRedecl env id 3
         (rEnv, rType) = checkArrayWithDimension env ty id
         const = case modal of 
                 ModConst  -> True
                 otherwise -> False

checkDeclIdent :: Env -> BIdent -> (Env, Maybe EnvRecord)
checkDeclIdent env id@(BIdent (pos,_))=
    case isDeclVar env id  of
        Just varInfos -> (env, Just varInfos)
        Nothing       -> (addError env (NotDeclared id, pos), Nothing)

checkExprsAndParams :: Env -> [Expr] -> [Param] ->BIdent -> Pos -> Type -> (Env, ExprRes)
checkExprsAndParams  env exprs params id@(BIdent (pos,_)) posDef retTy
    | length exprs /= length params = (addError envWithError(FuncArgsMismatch exprTypes paramsTypes id, pos), exprError)
    | not areComp = (addError env (FuncTypesMismatch exprTypes paramsTypes id posDef, pos), exprError)
    | otherwise = (refEnv, (ExprRes retTy False Nothing False))
    where listExprType =  map (saExpr (envWithoutErrors env) ) exprs 
          envs = map fst listExprType
          envWithError = env {errors = errors env ++(foldr (++) [] ( map errors envs) )}
          paramsTypes = [ty | (Parameter _ _ ty)<- params]
          exprTypes = [ty | (_,(ExprRes ty _ _ _ ))<- listExprType]
          areComp =  all (==True) (zipWith (leqType) exprTypes paramsTypes)
          refEnv = checkModalParameters envWithError exprs ([const | (_,(ExprRes _ _ _ const))<- listExprType]) params id 1

checkModalParameters :: Env ->[Expr]-> [Bool] -> [Param] -> BIdent ->Int ->Env
checkModalParameters env (e:exprs) (c:consts) (p:params) id parNum
    |constError = checkModalParameters addressEnv exprs consts params id (parNum +1)
    |c && (not constPass) = checkModalParameters (addError env (NonConstPass pos parNum id, pos)) exprs consts params id (parNum +1)
    |not refPassR && exprType /= ttype && not (areErrors exprType ttype) = checkModalParameters (addError env (NonEqRefTypes pos parNum id ttype exprType, pos)) exprs consts params id (parNum +1)
    |refPassR = checkModalParameters (addError env (NonRefLeft pos parNum id, pos)) exprs consts params id (parNum +1)
    |otherwise = checkModalParameters env exprs consts params id (parNum +1)
    where constPass = case p of 
                    (Parameter _ ModConst _) -> True
                    (Parameter _ ModVal _) -> True
                    otherwise -> False
          (refPassR,ttype) = case p of
                    (Parameter _ ModRef typ) ->case e of
                                  (ExprLeft _) -> (False, typ)
                                  (ExprAddress _) -> (False, typ)
                                  otherwise -> (True, typ)
                    (Parameter _ ModValRes typ) ->case e of
                                  (ExprLeft _) -> (False, typ)
                                  (ExprAddress _) -> (False, typ)
                                  otherwise -> (True, typ)
                    otherwise -> (False, ErrorType)
          (_, (ExprRes exprType _ _ _)) = saExpr env e
          pos = firstExprPos e
          (addressEnv,constError)=  checkAddressOfConst env c e pos 2
checkModalParameters env _ _ _ _ _= env

checkCaseExpr :: Env -> Type ->Expr -> Env 
checkCaseExpr env ty expr = 
  case equalitySupport ty of
    True -> env
    False -> addError env (CaseExprNotEq pos ty, pos)
  where pos = firstExprPos expr

checkUnaryTypes :: Env -> Type -> ExprRes -> Pos -> (Env, ExprRes)
checkUnaryTypes env typ eRes pos = 
  case typ of
          ErrorType -> (env, eRes)
          BasicType TypeInt -> (env, eRes)
          BasicType TypeReal -> (env, eRes)
          PointerType _ -> 
              (addError env (PointerArithmetic pos, pos), exprError)
          Array _ _ ->
              (addError env (PointerArithmetic pos, pos), exprError)
          otherwise -> 
              (addError env (NonNumericValue pos, pos), exprError)

checkAddressOfConst :: Env -> Bool -> Expr -> Pos -> Int -> (Env, Bool)
checkAddressOfConst env const expr pos kind
    | isAddressOfConst && kind==2 = (addError env (PassingRefOfConst pos, pos), True)
    | isAddressOfConst = (addError env (AssignConstToVar pos, pos), True)
    | otherwise = (env, False)
    where (_, (ExprRes ty _ _ _)) = saExpr env expr
          isAddressOfConst = case kind of 
                                  0 -> const && (case expr of
                                                    (ExprAddress _) -> True
                                                    otherwise -> False
                                                || case ty of
                                                    --Array _ _ -> True
                                                    otherwise -> False)
                                  otherwise -> const && (case ty of
                                                    PointerType _ -> True
                                                    --Array _ _-> True
                                                    otherwise -> False
                                                || case expr of 
                                                    (ExprAddress _) -> True
                                                    otherwise -> False)

checkModifyConstant:: Env -> BIdent -> Bool -> Env 
checkModifyConstant env id@(BIdent (pos@(_),_)) constLE 
    |constLE   = addError env (ChangingConstVar id, pos)
    |otherwise = env

checkBooleanCondition :: Env -> Expr -> Env
checkBooleanCondition env ex
    |exprType == BasicType TypeBool || exprType == ErrorType = exprEnv
    |otherwise = addError exprEnv (NonBooleanExpr pos exprType, pos)
    where pos = firstExprPos ex
          (exprEnv, eRes@(ExprRes exprType cTime valE _)) = saExpr env ex

convertToRedArray ::Env -> Type -> (Env,Type, Bool)
convertToRedArray env (ArrayType _ ty _) = (rEnv,Array rType 0, errorFounds)
  where (rEnv, rType,errorFounds) = convertToRedArray env ty

convertToRedArray env (ArrayDimType _ ty _ expr) 
  | isConst   = (rEnv2, Array (rType) (trust value), errorFounds)
  | otherwise = (rEnv2, Array rType 0, True)
  where (rEnv, isConst, value) = checkConstExpr env expr
        (rEnv2, rType, errorFounds)= convertToRedArray rEnv ty
convertToRedArray env ty = case ty of
      PointerType pt -> (nEnv, (PointerType nType), errorFounds )
        where (nEnv, nType, errorFounds) = convertToRedArray env pt
      otherwise ->  (env, ty, False)

checkArrays :: Type -> Type ->  Type
checkArrays decl rType = 
  case decl of 
    Array aType 0 ->
        case rType of
          Array ty dim ->  Array (checkArrays aType ty) dim
          otherwise -> decl
    PointerType ty -> 
        case rType of
          PointerType tty -> PointerType (checkArrays ty tty)
          otherwise -> decl
    otherwise -> decl

checkArrayWithDimension :: Env -> Type -> BIdent-> (Env, Type)
checkArrayWithDimension env ty id@(BIdent (pos,_))=
  case ty of
      ArrayType _ _ _ -> (addError nEnv (FunctionWithoutDimension id, pos), rTy)
      otherwise -> (nEnv, rTy)
     where (nEnv,rTy,_) = convertToRedArray env ty


checkReturnInitialized :: Env -> BIdent -> Env
checkReturnInitialized env id =
  case  isDeclVar env id of
    Nothing -> env
    Just z -> 
      case valRecord of
                Just y | y==z -> nEnv
                       | otherwise -> env
                otherwise ->  env
      where (lnames, maybeVar,retBool,boolFather,break, def) = funcCheck env
            nEnv = env {funcCheck = (lnames, bid, retBool || break, boolFather || break,break,def)}
            (bid, vName) = case maybeVar of
                              Just id@(BIdent (_,name)) -> (Just id, name)
                              otherwise-> (Nothing, "");
            valRecord = case lnames of
                          Nothing -> Nothing
                          Just x-> Data.Map.lookup vName x
checkMain :: Env -> Env
checkMain env =
  case getValue env "main" of
    Nothing -> addError env (NoMain (0,0),(0,0))
    otherwise -> env


checkStmtFor :: Env -> Bool -> Type -> Pos -> String -> Env
checkStmtFor env ctime ty pos kind = env2
  where intComp | kind == "ex1" = ty == (BasicType TypeInt)
                | otherwise = leqType ty (BasicType TypeInt)
        env1 | intComp = env
             | otherwise = addError env (NotIntegerType pos ty, pos)
        env2 | ctime || not intComp = env1
             | otherwise = addError env1 (NotCompileTime pos, pos)


checkDetLoop :: Env -> Pos -> Env
checkDetLoop  env  pos
  |val = addError env (InfiniteLoop pos, pos)
  |otherwise = env
  where (_,_,_,_,val,_)= funcCheck env



-------------------------------------------------          
--------- GETTER/SETTER/QUERIES/ HELPERS --------
-------------------------------------------------

returnInit :: Env ->  BIdent -> Env 
returnInit env id =
  env {funcCheck = (Just (names env), Just id, False, False,True, False)}

copyErrors :: Env -> Env -> Env 
copyErrors dest source = dest {errors = (errors dest)++(errors source)}

isSymbolDeclared::Env -> Id -> Bool
isSymbolDeclared env id = member id (names env) 

addSymbol:: Env -> Id -> EnvRecord -> Env
addSymbol env name record  =
    env {names = Map.insert name record (names env)}

addError :: Env -> (Error,Pos) -> Env
addError env (err,pos) = 
    env {errors = (es++[(errorType err, pos)]) }
    where es = errors env;

addIfNotDeclared :: Env -> BIdent -> Int -> EnvRecord -> Env
addIfNotDeclared env id@(BIdent (_,name)) errorKind symbolInfo
    | redecl = envR
    | otherwise = addSymbol envR name symbolInfo
    where (envR, redecl) = checkEnvRedecl env id errorKind

createEnv :: Env -> Env
createEnv oldEnv = Env (Map.fromList defaultFuncList) (Just oldEnv) [] (loop oldEnv) (funcCheck oldEnv) (loopFor oldEnv)

getValue :: Env -> Id -> Maybe EnvRecord
getValue env key = Data.Map.lookup key (names env)

isDeclVar :: Env -> BIdent -> Maybe EnvRecord
isDeclVar env id@(BIdent (_,name))
    | isSymbolDeclared env name =  getValue env name
    | otherwise = case parent env of 
                         Just pEnv -> isDeclVar pEnv id
                         Nothing -> Nothing
getExprLen :: Env -> Expr -> Int
getExprLen env e =
    case e of
      ExprArray _ (x:xs) -> length(x:xs)
      ExprCreate _ _ _ dim -> 
          case saExpr env dim of
               (_,(ExprRes _ _ (Just x) _)) -> x
               otherwise -> 0
      otherwise -> 
          case saExpr env e of
            (_, (ExprRes (Array _ dim) _ _ _)) -> dim
            otherwise -> 0


maybeOp :: TACOp -> Maybe Int -> Maybe Int -> Maybe Int
maybeOp "+" (Just x) (Just y)  = Just (x+y)
maybeOp "*" (Just x) (Just y)  = Just (x*y)
maybeOp "-" (Just x) (Just y)  = Just (x-y)
maybeOp "/" (Just _) (Just _)  = Nothing
maybeOp "%" (Just x) (Just 0)  = Nothing
maybeOp "%" (Just x) (Just y)  = Just (mod x y)
maybeOp "^" (Just x) (Just y)  = Just (x^y)
maybeOp _ _ _ = Nothing

maybeNegative :: Maybe Int -> Maybe Int
maybeNegative (Just x) = (Just (-x))
maybeNegative Nothing = Nothing 


translateParamsToRed :: [Param] -> [Param]
translateParamsToRed [] = []
translateParamsToRed ((Parameter id mod ty):par) = (Parameter id mod tyy):translateParamsToRed par
  where (_,tyy,_) = convertToRedArray defaultEnv ty


copyEnvInfo :: Env -> Env -> Maybe Env-> Env
copyEnvInfo dest source (Just source2) =
    dest {errors = (errors dest)++(errors source)++(errors source2),
          funcCheck = (oldN, oldId,  valSource && valSource2, valOld || (valSource && valSource2),
          break && b1 && b2, def)}
    where (oldN,oldId,valOld,_,break , def) = funcCheck dest
          (_,_,valSource,_,b1,_) = funcCheck source
          (_,_,valSource2,_,b2,_) = funcCheck source2

copyEnvInfo dest source Nothing = 
  dest {errors = (errors dest)++(errors source), 
        funcCheck = (oldN, oldId,  valSource , valOld || valSource,
          break, def)}
  where (oldN,oldId,valOld,_,break, def) = funcCheck dest
        (_,_,valSource,_,_,_)            = funcCheck source

----------------------------------------------------------------------
-------------------- FOR CO-DEFINITIONS ------------------------------
----------------------------------------------------------------------

addFuncToEnv :: Env -> Decl -> Env
addFuncToEnv env (FuncDef id@(BIdent (pos,_)) params _ retTy _ _) =
    addIfNotDeclared env id 2 (FuncInfo outType (translateParamsToRed params) pos )
    where (_, outType, _) =  convertToRedArray env retTy

addFuncToEnv env (ProcDef id@(BIdent (pos,_)) params _ _) =
    addIfNotDeclared env id 2 (FuncInfo VoidType (translateParamsToRed params) pos )
addFuncToEnv env _ = env

addFuncsFromDecls :: Env -> [Decl] -> Env
addFuncsFromDecls env [] = env
addFuncsFromDecls env (d:ds) =
    addFuncsFromDecls (addFuncToEnv env d) ds

addFuncsFromStmts :: Env -> [Stmt] -> Env
addFuncsFromStmts env [] = env
addFuncsFromStmts env ((StmtDecl d):ss) =
    addFuncsFromStmts (addFuncToEnv env d) ss
addFuncsFromStmts env (_:ss) = addFuncsFromStmts env ss

envWithoutErrors :: Env -> Env
envWithoutErrors env = env {errors = []}

----------------------------------------------------------------------
-------------------- PRETTY-ENV (PRINTER) ----------------------------
----------------------------------------------------------------------
printEnv :: Env -> String

printEnv (Env names Nothing  _ isLoopBlock funche _ )
  = 
  "Names          = " ++  showNames (toList names) ++
  "\nIsLoopBlock  =  " ++ show isLoopBlock ++
  "\nFuncCheck    = " ++ show funche
printEnv (Env names (Just env)  _ isLoopBlock funche _)
  = "Names         = " ++  showNames (toList names) ++
    "\nParent      = " ++ printEnv  env ++
    "\nIsLoopBlock =  " ++ show isLoopBlock ++
    "\nFuncCheck   = " ++ show funche

showNames :: [(Id,EnvRecord)] -> String
showNames [] = "\n"
showNames ((id,envR):s) = "\n\t" ++ colorYellow id ++
  showEnvRecord envR ++ showNames s

showEnvRecords :: [EnvRecord] -> String
showEnvRecords [] = "\n"
showEnvRecords (x:xs) = "\n\t" ++
  showEnvRecord x ++ showEnvRecords xs

showEnvRecord :: EnvRecord -> String
showEnvRecord envR = case envR of
  VarInfo type_ const _  Nothing _ ->
    " Var "++  showType type_ ++ " const " ++ show const 
  VarInfo type_ const _ (Just a ) _->
    " Var " ++ showType type_ ++ " const " ++ show const ++
    " init " ++ show a
  FuncInfo rety params _ ->
    " func "++"\n\tReturn Type " ++ showType rety
    ++" \n\tParam :  " ++ showParams params ++ "\n"

showParams :: [Param] -> String
showParams [] = ""
showParams (x:xs) = showParam x ++ showParams xs

showParam :: Param -> String
showParam x = case x of
  Parameter ident mod type_ ->
    getId ident ++ " " ++ show mod ++ " " ++
    showType type_ 
