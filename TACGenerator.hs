module TACGenerator where

import Data.Map (Map, member, insert, lookup)
import qualified Data.Map as Map
import AbsRnb
import Utils


data State = State {
  -- explicit state
  counterTmp    :: Int,
  counterLabel  :: Int,
  counterString :: Int,
  -- inherited
  env           :: Env,
  next          :: TACLabel,
  ttff          :: (TACLabel, TACLabel),
  bbcc          :: (TACLabel, TACLabel),
  returnLab     :: TACLabel,
  onException   :: TACLabel,
  -- tac code
  tac           :: [TAC],
  functions     :: [[TAC]],
  static        :: Map String TACAddr
  } deriving (Show)

--------------------------------------------

data Env = Env {
  names  :: Map Id ElemAttrs,
  parent :: Maybe Env
  } deriving (Eq, Show)

data ElemAttrs
  = ExprAttrs {
       eAddr  :: TACAddr,
       eTy    :: Type
       }
  | FuncAttrs {
      callAt  :: TACLabel,
      parMods :: [Param],
      retTy   :: Type
      }
  deriving (Show, Eq)

--------------------------------------------------------------------
------------------------ TAC GENERATION ----------------------------
--------------------------------------------------------------------

generateTAC :: Program -> [TAC]
generateTAC prog =  (reverse $ tac state) ++ (getStaticTAC state)
  where state = genProgram prog

------------------------------------------------------------------
------------------------ PROGRAM ---------------------------------

genProgram :: Program -> State
genProgram (Start ds) = foldFuncs $ addTAC mainState TACExit
  where state = genDecls initState ds
        mainState | isDeclared state "main" = addTAC state (TACCallProc mainLab)
                  | otherwise               = state
        mainLab = callAt $ findSymbol state "main"

-------------------------------------------------------------------
------------------------ DECLARATIONS -----------------------------

genDecls :: State -> [Decl] -> State
genDecls state ds = genDefs (addFuncsFromDecls state ds) ds

genDefs :: State -> [Decl] -> State
genDefs state []     = state
genDefs state (d:ds) = genDefs (genDef state d) ds

genDef :: State -> Decl -> State
genDef state decl = case decl of
  VarDecl bId ty -> addSymbol state2 (getId bId, elem)
    where idAddr  = getIdAddress bId
          (ExprAttrs defAddr _, state1) = genExpr state $ defaultValue ty
          state2  = addTAC state1 $ TACAssign idAddr ty defAddr
          elem    = ExprAttrs idAddr ty

  VarDef bId ty _ expr -> addSymbol state1 (getId bId, elem)
    where idAddr = getIdAddress bId
          elem = ExprAttrs idAddr completeTy
          completeTy = getCompleteType state ty expr
          state1 = assignVariable state elem expr

  ConstDef bId ty _ expr -> addSymbol newstate (getId bId, elem)
    where idAddr = getIdAddress bId
          elem   = ExprAttrs addr completeTy
          completeTy = getCompleteType state ty expr
          state2 = assignVariable state (ExprAttrs idAddr completeTy) expr
          -- If constant value, load it.
          (ExprAttrs eAddr eTy) = fst $ genExpr state expr
          (addr, newstate) = case eAddr of
            ValAddr _ -> (ValAddr cAddr, addTAC (inheritStatic state state2) $ TACComment comment)
              where comment = "Constant "++ name ++" has value "++ cAddr
                    (ValAddr cAddr) = maybeConvertValAddr eAddr eTy completeTy
                    Addr name = idAddr
            otherwise -> (idAddr, state2)

  FuncDef bId params retId ty body _ -> newstate
    where callLab = callAt $ findSymbol state (getId bId)
          funSt   = state { tac = [], functions = [] }
          -- Preamble: load parameters (input and output)
          funSt1 = addTAC (addTAC funSt (TACAddLabel callLab))
                          (TACComment ("Preamble "++(getId bId)))
          funSt2 = addInitVar (addParamsToEnv funSt1 params) retId ty
          funSt3 = pushStmtsEnv (addTAC funSt2
                          (TACComment ("Body "++(getId bId)))) body
          (retLab, funSt4) = newLabel funSt3
          funSt5 = genStmts (setNext (setReturnLab funSt4 retLab) retLab) body
          -- Postamble: return params.
          funSt6 = addTAC (addTAC funSt5 (TACAddLabel retLab))
                          (TACComment ("Postamble "++(getId bId)))
          funSt7 = addTAC (copyBackParams funSt6 params)
                          (TACReturn (getIdAddress retId))
          state1 = inheritState state funSt7
          newstate = addFunctionTAC state1 (tac $ foldFuncs funSt7)

  ProcDef bId params body _ -> newstate
    where callLab = callAt $ findSymbol state (getId bId)
          funSt  = state { tac = [], functions = [] }
          -- Preamble: load parameters (input and output)
          funSt1 = addTAC (addTAC funSt (TACAddLabel callLab))
                          (TACComment ("Preamble "++(getId bId)))
          funSt2 = addParamsToEnv funSt1 params
          funSt3 = pushStmtsEnv (addTAC funSt2
                                (TACComment ("Body "++(getId bId)))) body
          (retLab, funSt4) = newLabel funSt3
          funSt5 = genStmts (setNext (setReturnLab funSt4 retLab) retLab) body
          -- Postamble: return params.
          funSt6 = addTAC (addTAC funSt5 (TACAddLabel retLab))
                          (TACComment ("Postamble "++(getId bId)))
          funSt7 = addTAC (copyBackParams funSt6 params)
                          (TACReturn NoAddr)
          state1 = inheritState state funSt7
          newstate = addFunctionTAC state1 (tac $ foldFuncs funSt7)


--------------------------------------------------------------------
-------------------------- STATEMENTS ------------------------------

genStmts :: State -> [Stmt] -> State
genStmts state []   = state
genStmts state [s1] = genStmt state s1
genStmts state (s1:s2:ss) | isFuncDecl s2 = genStmts (genStmt state s1) (s2:ss)
genStmts state (s1:ss) = newstate
  where ssnext = next state
        (s1next, state1) = newLabel state
        state2 = genStmt (setNext state1 s1next) s1
        state3 = addTAC state2 $ TACAddLabel s1next
        newstate = genStmts (setNext state3 ssnext) ss

genStmt :: State -> Stmt -> State
genStmt state stmt = case stmt of
  StmtAssign lexpr _ rexpr -> newstate
    where -- Evaluate from left to right.
          (leRes, state1) = genLExpr state lexpr
          newstate = assignVariable state1 leRes rexpr
  StmtIf guard thenBranch elseStmts -> genCond state guard thenBranch elseStmts
  StmtLoop _ body _       -> genLoop state body
  StmtLoopCmd loopCmd     -> genLoopCmd state loopCmd
  StmtFor i ty _ init d end body -> genForLoop state i ty init d end body
  StmtCase _ expr cases _ -> genCases state expr cases
  StmtTryCatch _ try _ catch _ -> genTryCatch state try catch
  StmtReturn  _           -> gotoReturn state
  StmtLocal stmts         -> popEnv $ genStmts (pushStmtsEnv state stmts) stmts
  StmtDecl decl           -> genDef state decl
  StmtExpr expr           -> snd $ genExpr state expr


--------------------------------------------------------------------
-- Local:

genLocalStmts :: State -> [Stmt] -> State
genLocalStmts state body = popEnv $ genStmts (pushStmtsEnv state body) body

--------------------------------------------------------------------
-- Loop:

genLoop :: State -> [Stmt] -> State
genLoop state body = newstate
  where nextLab            = next state
        (bbLab, ccLab)     = bbcc state
        (beginLab, state1) = newLabel state
        (endLab, state2)   = newLabel state1
        state3 = addTAC state2 $ TACAddLabel beginLab
        state4 = setBBCC (pushStmtsEnv state3 body) nextLab beginLab
        state5 = genStmts (setNext state4 endLab) body
        state6 = setBBCC (popEnv state5) bbLab ccLab
        newstate = addTAC (addTAC state6 (TACAddLabel endLab))
                           (TACGoto beginLab)

genLoopCmd :: State -> LoopCmd -> State
genLoopCmd state cmd = case cmd of
  LoopExitOn _ cond -> genControlFlow (setTTFF state exitLab Fall) cond
    where exitLab = fst $ bbcc state
  LoopExit _        -> gotoBB state
  LoopContinue _    -> gotoCC state


genForLoop :: State -> BIdent -> BType -> Expr -> Dir -> Expr -> [Stmt] -> State
genForLoop state bId bTy init dir end body = newstate
  where idAddr = getIdAddress bId
        ty = BasicType bTy
        elem = ExprAttrs idAddr ty
        state1 = assignVariable state elem init
        state2 = addSymbol (pushStmtsEnv state1 body) (getId bId, elem)
        (ExprAttrs endAddr endTy, state3) = genExpr state2 end
        (endAddr1, state4) = getSimpleAddr state3 endAddr endTy
        state5 = addTAC state4 $ TACGoto guardLab
        (beginLab, state6) = newLabel state5
        (guardLab, state7) = newLabel state6
        (endLab, state8)   = newLabel state7
        state9 = genStmts (setNext (addTAC state8 $ TACAddLabel beginLab) endLab) body
        state10 = snd $ genExpr (addTAC state9 (TACAddLabel endLab)) step
        state11 = addTAC (addTAC state10 $ TACAddLabel guardLab)
                        (TACIfRel idAddr rel ty endAddr1 beginLab)
        newstate = popEnv state11
        ---
        step | dir == FromTo = preIncr_ bId
             | dir == DownTo = preDecr_ bId
        rel  | dir == FromTo = relLEq_
             | dir == DownTo = relGEq_


--------------------------------------------------------------------
-- Conditionals:

genCond :: State -> Expr -> [Stmt] -> ElseStmt -> State
genCond state guard tBranch e@StmtNoElse = newstate
  where nextLab  = next state
        state1   = genControlFlow (setTTFF state Fall nextLab) guard
        newstate = genLocalStmts state1 tBranch
genCond state guard tBranch elseStmts = case elseStmts of
  StmtElseIf guard1 then1 else1 -> genCond state5 guard1 then1 else1
  StmtElse eBranch   -> genLocalStmts state5 eBranch
  where nextLab           = next state
        (elseLab, state1) = newLabel state
        state2 = genControlFlow (setTTFF state1 Fall elseLab) guard
        state3 = popEnv $ genStmts (pushStmtsEnv state2 tBranch) tBranch
        state4 = addTAC (setNext state3 nextLab) (TACGoto nextLab)
        state5 = addTAC state4 $ TACAddLabel elseLab

--------------------------------------------------------------------
-- Cases:

genCases :: State -> Expr -> Cases -> State
genCases state expr cases
  = exploreCases newstate (ExprAttrs addr exTy) cases
  where (ExprAttrs exAddr exTy, state1) = genExpr state expr
        (addr, newstate) = case exAddr of
          ValAddr _ -> (exAddr, state)
          otherwise -> (tmpAddr, state3)
        (tmpAddr, state2) = newTmp state1
        state3 = addTAC state2 $ TACAssign tmpAddr exTy exAddr

exploreCases :: State -> ElemAttrs -> Cases -> State
exploreCases state _ CaseEps = state
exploreCases state eRes cases = case cases of
  CaseDefault body          -> genLocalStmts state body
  CaseVal vExpr body cases1 -> exploreCases newstate eRes cases1
    where nextLab = next state
          (ExprAttrs vAddr vTy, state1) = genExpr state vExpr
          (ExprAttrs eAddr eTy) = eRes
          ty = getMaxType vTy eTy
          (vS, state2) = maybeGetSimpleAddr state1 vAddr vTy
          (eS, state3) = maybeGetSimpleAddr state2 eAddr eTy
          (vC, state4) = maybeConvert state3 vS vTy ty
          (eC, state5) = maybeConvert state4 eS eTy ty
          newVal = dispatchBoolRel ty (RelEq eq_) eC vC
          state6 = addTAC state5 $ TACIfRel vC (RelNotEq neq_) ty eC nextVal
          bodyState = case newVal of
            ValAddr "True"  -> inheritStatic state state6
            ValAddr "False" -> addTAC (inheritStatic state state6) $ TACGoto nextVal
            otherwise -> state6
          (nextVal, bodyState1) = newLabel bodyState
          bodyState2 = addTAC (genLocalStmts bodyState1 body) (TACGoto nextLab)
          newstate = addTAC bodyState2 $ TACAddLabel nextVal


--------------------------------------------------------------------
-- Try Catch:

genTryCatch :: State -> [Stmt] -> [Stmt] -> State
genTryCatch state try catch = newstate
  where nextLab   = next state
        prevOnExc = onException state
        (catchLab, state1) = newLabel state
        state2 = addTAC (genLocalStmts
                 (setOnException state1 catchLab) try) $ TACGoto nextLab
        state3 = addTAC state2 $ TACAddLabel catchLab
        newstate = genLocalStmts (setOnException state3 prevOnExc) catch



--------------------------------------------------------------------
------------------------- EXPRESSIONS ------------------------------

genExpr :: State -> Expr -> (ElemAttrs, State)
genExpr state expr = case expr of
  ExprVal val             -> genVal state val
  ExprArray _ exprs       -> genArrayExpr state exprs
  ExprCreate _ ty _ dim   -> genArrayCreate state ty dim
  ExprAddress lexpr       -> genAddressOf state lexpr
  ExprLeft lexpr          -> genLExpr state lexpr
  ExprCall bId exprs      -> genCall state (getId bId) exprs
  ExprBrack expr          -> genExpr state expr
  ExprPostIncr lexpr      -> genPostOp state lexpr "add"
  ExprPostDecr lexpr      -> genPostOp state lexpr "sub"
  ExprPow expr1 _ expr2   -> genExprBinOp state expr1 expr2 "pow"
  ExprMul expr1 _ expr2   -> genExprBinOp state expr1 expr2 "mul"
  ExprMod expr1 _ expr2   -> genExprBinOp state expr1 expr2 "mod"
  ExprDiv expr1 _ expr2   -> genExprBinOp state expr1 expr2 "div"
  ExprSum expr1 _ expr2   -> genExprBinOp state expr1 expr2 "add"
  ExprMinus expr1 _ expr2 -> genExprBinOp state expr1 expr2 "sub"
  ExprUnaryMinus _ expr1  -> genExprUnOp state expr1 "negate"
  ExprUnaryPlus _ expr1   -> genExprUnOp state expr1 "+"
  ExprIf expr1 cond expr2 -> genExprIf state expr1 cond expr2
  otherwise               -> genBoolExpr state expr


---------------------------------------------------------------------
-- Values:

genVal :: State -> Val -> (ElemAttrs, State)
genVal state v@(StringVal bString) = addStaticString state bString
genVal state value = (elem, state)
  where elem = ExprAttrs (ValAddr val) ty
        (val, ty) = case value of
          IntVal (BInteger ((_,_), v)) -> (v, int_)
          RealVal (BReal ((_,_), v))   -> (v, real_)
          CharVal (BChar ((_,_), v))   -> (v, char_)
          BoolVal (BBool ((_,_), v))   -> (v, bool_)


---------------------------------------------------------------------
-- Left Expressions:

genLExpr :: State -> LeftExpr -> (ElemAttrs, State)
genLExpr state lexpr = (elem, newstate)
  where baseAttrs = findSymbol state (getName lexpr)
        (ExprAttrs baseAddr baseTy) = baseAttrs
        (elem, newstate) = case lexpr of
          LEId _ -> (baseAttrs, state)

          LEDeref inLexpr
                 -> derefAttrs state1 leRes
            where (leRes, state1) = genLExpr state inLexpr

          LEArray inLexpr (DimArray _ iExpr)
                 -> (ExprAttrs addr elemTy, newstate)
            where elemTy = getArrayElemType leTy
                  (ExprAttrs leAddr leTy, state1) = genLExpr state inLexpr
                  (ExprAttrs iAddr iTy, state2) = genExprBinOp state1 iExpr (expr_sizeOfType elemTy) "mul"
                  (cAddr, state3) = maybeConvert state2 iAddr iTy int_
                  (accAddr, state4) = accessAt state3 leAddr cAddr
                  (addr, newstate) = case elemTy of
                    Array _ _ -> pointerToAddr state4 accAddr
                    otherwise -> (accAddr, state4)

          LEPreIncrem inLexpr -> genPreOp state inLexpr "add"

          LEPreDecr inLexpr   -> genPreOp state inLexpr "sub"

          LEBra inLexpr       -> genLExpr state inLexpr

---------------------------------------------------------------------
-- AddressOf: (§)

genAddressOf :: State -> LeftExpr -> (ElemAttrs, State)
genAddressOf state lexpr = (elem, newstate)
  where (ExprAttrs baseAddr baseTy) = findSymbol state (getName lexpr)
        elem = ExprAttrs addr ty
        (ExprAttrs lexAddr lexTy, state1) = genLExpr state lexpr
        (addr, newstate) = pointerToAddr state1 lexAddr
        ty = pointerToType lexTy

---------------------------------------------------------------------
-- Pre incr / decr:

genPreOp :: State -> LeftExpr -> TACOp -> (ElemAttrs, State)
genPreOp state lexpr op = (leRes, newstate)
  where (leRes, state1) = genLExpr state lexpr
        (ExprAttrs leAddr leTy) = leRes
        oneAddr  = unityAddr leTy
        (sAddr, state2) = maybeGetSimpleAddr state1 leAddr leTy
        (tmpAddr, state3) = newTmp state2
        state4 = addTAC state3 $ TACBinOp tmpAddr sAddr op leTy oneAddr
        newstate = addTAC state4 $ TACAssign leAddr leTy tmpAddr

----------------------------------------------------------------------
-- Arrays:

genArrayCreate :: State -> Type -> Expr -> (ElemAttrs, State)
genArrayCreate state ty dimExpr = (resArray, newstate)
  where (ExprAttrs nAddr nTy) = fst $ genExpr state dimExpr
        dim = getIntValue $ fst $ maybeConvert state nAddr nTy int_
        vals = defaultValsList ty dim
        (baseAddr, state2) = newTmp state
        -- Builds an array initialized with a list of Default Values.
        (base0, state3) = accessAt0 state2 baseAddr
        newstate = snd $ buildArray state3 ty vals base0
        resArray = ExprAttrs baseAddr (Array ty dim)

genArrayExpr :: State -> [Expr] -> (ElemAttrs, State)
genArrayExpr state [] = (emptyRes, state)
genArrayExpr state exprs@(e:_) = (resArray, newstate)
  where expectedTy = eTy $ fst $ genExpr state e
        (baseAddr, state1) = newTmp state
        (base0, state3) = accessAt0 state1 baseAddr
        newstate = snd $ buildArray state3 expectedTy exprs base0
        resArray = ExprAttrs baseAddr (Array expectedTy (length exprs))

---------------------------------------------------------------------
-- Arithmetic Operations:

genExprBinOp :: State -> Expr -> Expr -> TACOp -> (ElemAttrs, State)
genExprBinOp state xE yE op =  (elem, newstate)
  where elem = ExprAttrs addr ty
        (addr, newstate) = case newVal of
          ValAddr _  -> (newVal, state)
          otherwise  -> (tmpAddr, state8)
        -- Left-to-right evaluation in expressions.
        (ExprAttrs xAddr xTy, state1) = genExpr state xE
        (xS, state2) = getSimpleAddr state1 xAddr xTy
        (ExprAttrs yAddr yTy, state3) = genExpr state2 yE
        ty | op == "div" = real_
           | op == "pow" = int_
           | op == "mod" = int_
           | otherwise   = getMaxType (getMaxType xTy yTy) int_
        (yS, state4) = maybeGetSimpleAddr state3 yAddr yTy
        (xC, state5) = maybeConvert state4 xS xTy ty
        (yC, state6) = maybeConvert state5 yS yTy ty
        (tmpAddr, state7) = newTmp state6
        state8 = addTAC state7 $ TACBinOp tmpAddr xC op ty yC
        newVal = dispatchBOp ty op xC yC

genExprUnOp :: State -> Expr -> TACOp -> (ElemAttrs, State)
genExprUnOp state xE op = (elem, newstate)
  where elem = ExprAttrs addr dstTy
        (addr, newstate) = case newVal of
          (ValAddr _) -> (newVal, state)
          otherwise   -> (tmpAddr, state5)
        (ExprAttrs xAddr xTy, state1) = genExpr state xE
        (sX, state2) = maybeGetSimpleAddr state1 xAddr xTy
        dstTy = getMaxType int_ xTy
        (cX, state3) = maybeConvert state2 sX xTy dstTy
        (tmpAddr, state4) = newTmp state3
        state5 = addTAC state4 $ TACUnOp tmpAddr op dstTy cX
        newVal = dispatchUOp xTy op sX

genPostOp :: State -> LeftExpr -> TACOp -> (ElemAttrs, State)
genPostOp state lexpr op = (tmpElem, newstate)
  where (ExprAttrs leAddr leTy, state1) = genLExpr state lexpr
        (tmpAddr, state2) = newTmp state1
        state3   = addTAC state2 $ TACAssign tmpAddr leTy leAddr
        tmpElem  = ExprAttrs tmpAddr leTy
        oneAddr  = unityAddr leTy
        (tmpAddr1, state4) = newTmp state3
        state5 = addTAC state4 $ TACBinOp tmpAddr1 tmpAddr op leTy oneAddr
        newstate = addTAC state5 $ TACAssign leAddr leTy tmpAddr1

----------------------------------------------------------------------
-- Booleans:

genBoolExpr :: State -> Expr -> (ElemAttrs, State)
genBoolExpr state expr = (elem, newstate)
  where elem = ExprAttrs bAddr bool_
        (bAddr, newstate) = case newVal of
          ValAddr _ -> (newVal, inheritStatic state state6)
          otherwise -> (tmpAddr, state6)
        (nextLab, state1) = newLabel state
        (ff, state2)      = newLabel state1
        state3 = genControlFlow (setTTFF state2 Fall ff) expr
        (tmpAddr, state4) = newTmp state3
        state5 = addTAC (addTAC state4
                        (TACAssign tmpAddr bool_ (ValAddr "True")))
                        (TACGoto nextLab)
        state6 = addTAC (addTAC (addTAC state5
                        (TACAddLabel ff))
                        (TACAssign tmpAddr bool_ (ValAddr "False")))
                        (TACAddLabel nextLab)
        -- Try to compute the value:
        newVal = case expr of
          ExprAnd e1 _ e2   -> genValBoolBOp state "&&" e1 e2
          ExprOr e1 _ e2    -> genValBoolBOp state "||" e1 e2
          ExprNot _ e1      -> genValBoolUOp state "!" e1
          ExprRel e1 rel e2 -> fst $ genExprRelOp state e1 rel e2


genControlFlow :: State -> Expr -> State
genControlFlow state expr = case expr of
  ExprVal (BoolVal val) -> case val of
    BBool ((_,_),"True")  | isFallTT state -> state
                          | otherwise      -> gotoTT state
    BBool ((_,_),"False") | isFallFF state -> state
                          | otherwise      -> gotoFF state
  ExprBrack expr1       -> genControlFlow state expr1
  ExprAnd expr1 _ expr2 -> newstate
    where (tt, ff) = ttff state
          (ff1, state1) | isFallFF state = newLabel state
                        | otherwise      = (ff, state)
          state2 = genControlFlow (setTTFF state1 Fall ff1) expr1
          state3 = genControlFlow (setTTFF state2 tt ff) expr2
          newstate | isFallFF state = addTAC state3 $ TACAddLabel ff1
                   | otherwise      = state3
  ExprOr expr1 _ expr2  -> newstate
    where (tt, ff) = ttff state
          (tt1, state1) | isFallTT state = newLabel state
                        | otherwise      = (tt, state)
          state2 = genControlFlow (setTTFF state1 tt1 Fall) expr1
          state3 = genControlFlow (setTTFF state2 tt ff) expr2
          newstate | isFallTT state = addTAC state3 $ TACAddLabel tt1
                   | otherwise      = state3
  ExprNot _ expr1      -> newstate
    where (tt, ff) = ttff state
          newstate = genControlFlow (setTTFF state ff tt) expr1
  ExprRel expr1 rel expr2 -> snd $ genExprRelOp state expr1 rel expr2

  otherwise -> case eAddr of
    ValAddr "True"  -> gotoTT state
    ValAddr "False" -> gotoFF state
    otherwise       -> newstate
    where (tt, ff) = ttff state
          (ExprAttrs eAddr eTy, state1) = genExpr state expr
          (cAddr, state2) = maybeConvert state1 eAddr eTy bool_
          newstate = case (tt, ff) of
            (Fall, Fall) -> state2
            (_, Fall)    -> addTAC state2 $ TACIf cAddr tt
            (Fall, _)    -> addTAC state2 $ TACIfFalse cAddr ff
            (_, _)       -> addTAC (addTAC state2 (TACIf cAddr tt)) (TACGoto ff)

genValBoolBOp :: State -> TACOp -> Expr -> Expr -> TACAddr
genValBoolBOp state op e1 e2 = boolBOp op (eAddr res1) (eAddr res2)
  where (res1, state1) = genExpr state e1
        (res2, state2) = genExpr state e2
genValBoolUOp :: State -> TACOp -> Expr -> TACAddr
genValBoolUOp state op e1 = boolUOp op (eAddr res1)
  where (res1, state1) = genExpr state e1

-- Returns a couple corresponding to
--    (valBoolRelOp, genControlFlow)
genExprRelOp :: State -> Expr -> Rel -> Expr -> (TACAddr, State)
genExprRelOp state xE rel yE = (newVal, inheritStatic newstate state7)
  where newstate = case newVal of
          ValAddr "True"  -> gotoTT state
          ValAddr "False" -> gotoFF state
          otherwise       -> state7
        (ExprAttrs xAddr xTy, state1) = genExpr state xE
        (xS, state2) = getSimpleAddr state1 xAddr xTy
        (ExprAttrs yAddr yTy, state3) = genExpr state2 yE
        ty = getMaxType xTy yTy
        (tt, ff) = ttff state
        (yS, state4) = maybeGetSimpleAddr state3 yAddr yTy
        (xC, state5) = maybeConvert state4 xS xTy ty
        (yC, state6) = maybeConvert state5 yS yTy ty
        newVal = dispatchBoolRel ty rel xC yC
        state7 = case (tt, ff) of
          (_, Fall) -> addTAC state6 $ TACIfRel xC rel ty yC tt
          (Fall, _) -> addTAC state6 $ TACIfRel xC (notRel rel) ty yC ff
          (_ , _)   -> addTAC (addTAC state6
                              (TACIfRel xC rel ty yC tt))
                              (TACGoto ff)

----------------------------------------------------------------------
-- If Expr:

genExprIf :: State -> Expr -> Expr -> Expr -> (ElemAttrs, State)
genExprIf state expr1 cond expr2 = (elem, newstate)
  where elem = ExprAttrs tmpAddr ty1
        (nextLab, state1) = newLabel state
        (ff, state2)      = newLabel state1
        state3 = genControlFlow (setTTFF state2 Fall ff) cond
        (tmpAddr, state4) = newTmp state3
        (ExprAttrs addr1 ty1, state5) = genExpr state4 expr1
        state6 = addTAC (addTAC state5
                        (TACAssign tmpAddr ty1 addr1))
                        (TACGoto nextLab)
        (ExprAttrs addr2 ty2, state7) = genExpr (addTAC state6 $ TACAddLabel ff) expr2
        newstate = addTAC (addTAC state7
                        (TACAssign tmpAddr ty2 addr2))
                        (TACAddLabel nextLab)

----------------------------------------------------------------------
-- Calls:

genCall :: State -> Id -> [Expr] -> (ElemAttrs, State)
genCall state id actuals = (res, newstate)
 where (FuncAttrs callLab formals retTy) = findSymbol state id
       state1 = setActualPars state actuals formals
       (res, newstate) = case retTy of
          VoidType  -> (emptyRes, state2)
            where state2 = addTAC state1 $ TACCallProc callLab
          otherwise -> (ExprAttrs tmpAddr retTy, state3)
            where (tmpAddr, state2) = newTmp state1
                  state3 = addTAC state2 $ TACCallFunc tmpAddr retTy callLab

setActualPars :: State -> [Expr] -> [Param] -> State
setActualPars state [] _ = state
setActualPars state (e:es) (p@(Parameter _ mod ty):ps)
  = setActualPars newstate es ps
  where newstate = addTAC state4 $ TACParam sAddr
        (sAddr, state4) = maybeGetSimpleAddr state3 (eAddr aRes) (eTy aRes)
        (aRes, state3)  = case mod of
          ModVal    -> (parRes, assignVariable cpState parRes e)
            where (parAddr, cpState) = newTmp state
                  parRes = ExprAttrs parAddr ty
          ModConst  -> (parRes, assignVariable cpState parRes e)
            where (parAddr, cpState) = newTmp state
                  parRes = ExprAttrs parAddr ty
          -- Ref, Res, ValRes
          otherwise -> (refAttrs, state2)
            where (refAttrs, state2) = pointerToAttrs refSt eRes
                  (eRes, refSt) = genExpr state e

--------------------------------------------------------------------
-------------------------  HELPERS ---------------------------------
--------------------------------------------------------------------


-- Collects functions' names and the `callAt' labels in order to allow mutual recursion.
addFuncToEnv :: State -> Decl -> State
addFuncToEnv state (FuncDef bId params _ retTy _ _)
  = addSymbol state (getId bId, funcAttr)
  where funcAttr = FuncAttrs (getIdLabel bId) tacPars (getTACType state retTy)
        tacPars = map (getParamTACType state) params
addFuncToEnv state (ProcDef bId params _ _)
  = addSymbol state (getId bId, funcAttr)
  where funcAttr = FuncAttrs (getIdLabel bId) tacPars VoidType
        tacPars = map (getParamTACType state) params
addFuncToEnv state decl = state    -- Not a function:

addFuncsFromDecls :: State -> [Decl] -> State
addFuncsFromDecls state [] = state
addFuncsFromDecls state (d:ds)
  = addFuncsFromDecls (addFuncToEnv state d) ds

addFuncsFromStmts :: State -> [Stmt] -> State
addFuncsFromStmts state [] = state
addFuncsFromStmts state ((StmtDecl d):ss)
  = addFuncsFromStmts (addFuncToEnv state d) ss
addFuncsFromStmts state (_:ss) = addFuncsFromStmts state ss


-----------------------------------------------------------------
-- Functions:

-- Function's preamble:
-- Builds the environment of the function with its parameters according to their modality.
addParamsToEnv :: State -> [Param] -> State
addParamsToEnv state [] = state
addParamsToEnv state (p@(Parameter bId mod bTy):ps)
  = addSymbol newstate (getId bId, parAttrs)
  where ty = getTACType state bTy
        addr = getIdAddress bId
        val = ExprAttrs addr ty
        local = ExprAttrs (localCopyAddr addr) ty
        ref | isAddressType ty = val
            | otherwise = ExprAttrs (derefAddr addr) ty
        (parAttrs, newstate) = case mod of
          ModRef    -> (ref, state)
          ModValRes -> (local, copyVariable state local ref)
          -- Value, const
          otherwise -> (val, state)

-- Adds to the environment the return variable initialized with its default value.
addInitVar :: State -> BIdent -> Type -> State
addInitVar state bId bTy = addSymbol state1 (getId bId, var)
  where var = ExprAttrs (getIdAddress bId) (getTACType state bTy)
        state1 = initializeVar state var
initializeVar :: State -> ElemAttrs -> State
initializeVar state (ExprAttrs addr ty) = case ty of
  BasicType t -> addTAC state1 (TACAssign addr ty defAddr)
    where (ExprAttrs defAddr _, state1) = genExpr state (defaultValue ty)
  otherwise   -> state


-- Function's postamble:
copyBackParams :: State -> [Param] -> State
copyBackParams state [] = state
copyBackParams state (p@(Parameter bId mod bTy):ps) = case mod of
  ModValRes -> newstate
  otherwise -> state
  where newstate = copyVariable state ref local
        addr = getIdAddress bId
        ty = getTACType state bTy
        local = ExprAttrs (localCopyAddr addr) ty
        ref | isAddressType ty = ExprAttrs addr ty
            | otherwise = ExprAttrs (derefAddr addr) ty


-----------------------------------------------------------------
-- Memory handlers:

assignVariable :: State -> ElemAttrs -> Expr -> State
assignVariable state baseAttrs expr = case expr of
  ExprArray _ exprs     -> snd $ buildArray state0 elemTy exprs base0
  ExprCreate _ ty _ dim -> snd $ buildArray state0 elemTy vals base0
    where (ExprAttrs nAddr nTy) = fst $ genExpr state dim
          n = getIntValue $ fst $ maybeConvert state nAddr nTy int_
          vals = defaultValsList ty n
  otherwise -> copyVariable state1 baseAttrs reRes
    where (reRes, state1) = genExpr state expr
  where (ExprAttrs baseAddr baseTy) = baseAttrs
        (base0, state0) = accessAt0 state baseAddr
        elemTy = getArrayElemType baseTy


copyVariable :: State -> ElemAttrs -> ElemAttrs -> State
copyVariable state dst src = case srcTy of
  Array ty n -> snd $ copyMemory state2 dst0 (getArrayElemType dstTy) src0 ty n
  otherwise  -> addTAC cState $ TACAssign dstAddr dstTy cAddr
  where (ExprAttrs dstAddr dstTy) = dst
        (ExprAttrs srcAddr srcTy) = src
        (dst0, state1) = accessAt0 state dstAddr
        (src0, state2) = accessAt0 state1 srcAddr
        (sAddr, sState) = maybeGetSimpleAddr state srcAddr srcTy
        (cAddr, cState) = maybeConvert sState sAddr srcTy dstTy


-- Copies from `src' to `dst' the `n' following elements of type `t'.
copyMemory :: State -> TACAddr -> Type -> TACAddr -> Type -> Int -> (TACAddr, State)
copyMemory state dst _ _ _ 0 = (dst, state)
copyMemory state dst dstTy src srcTy n = copyMemory newstate newdst dstTy newsrc srcTy (n-1)
  where (newdst, state4) = case srcTy of
          Array t1 n1 -> copyMemory state dst (getArrayElemType dstTy) src t1 n1
          otherwise -> arrayAdvance state3 dst (sizeOfType dstTy)
            where (sSrc, state1) = maybeGetSimpleAddr state src srcTy
                  (cSrc, state2) = maybeConvert state1 sSrc srcTy dstTy
                  state3 = addTAC state2 (TACAssign dst dstTy cSrc)
        (newsrc, newstate) = arrayAdvance state4 src (sizeOfType srcTy)



-- Actually builds an array initialized with a list of values.
buildArray :: State -> Type -> [Expr] -> TACAddr -> (TACAddr, State)
buildArray state _ [] base = (base, state)
buildArray state ty@(Array t n) (e:es) base
  = buildArray state1 ty es newbase
  where (newbase, state1) = case e of
          ExprArray _ e1                -> buildArray state t e1 base
          ExprCreate _ t1 _ dimExpr     -> buildArray state t vals base
            where (ExprAttrs nAddr nTy) = fst $ genExpr state dimExpr
                  dim = getIntValue $ fst $ maybeConvert state nAddr nTy int_
                  vals = defaultValsList t1 dim
          -- Variable:
          otherwise -> copyMemory state1 base t src (getArrayElemType srcTy) n
            where (src, state1) = accessAt0 state srcAddr
                  ExprAttrs srcAddr srcTy = fst $ genExpr state e
buildArray state ty (e:es) base
  = buildArray newstate ty es newbase
  where (ExprAttrs eAddr eTy, state1) = genExpr state e
        (sAddr, state2) = maybeGetSimpleAddr state1 eAddr eTy
        (cAddr, state3) = maybeConvert state2 sAddr eTy ty
        state4 = addTAC state3 $ TACAssign base ty cAddr
        (newbase, newstate) = arrayAdvance state4 base (sizeOfType ty)


------------------------------------------------------------------
-- Addresses:

getSimpleAddr :: State -> TACAddr -> Type -> (TACAddr, State)
getSimpleAddr state addr@(ValAddr a) ty = (addr, state)
getSimpleAddr state addr ty = (tmpAddr, newstate)
  where (tmpAddr, state1) = newTmp state
        newstate = addTAC state1 $ TACAssign tmpAddr ty addr

--------------

derefAddr :: TACAddr -> TACAddr
derefAddr (Addr addr) = DerefAddr addr

derefAttrs :: State -> ElemAttrs -> (ElemAttrs, State)
derefAttrs state (ExprAttrs addr@(Addr _) ty) = (ExprAttrs derAddr derTy, state)
  where derTy = derefType ty
        derAddr = derefAddr addr
derefAttrs state (ExprAttrs addr ty) = (ExprAttrs derAddr derTy, newstate)
  where derTy = derefType ty
        derAddr = derefAddr tmpAddr
        (tmpAddr, state1) = newTmp state
        newstate = addTAC state1 $ TACAssign tmpAddr ty addr

--------------

pointerToAddr :: State -> TACAddr -> (TACAddr, State)
pointerToAddr state (Addr addr)      = (PointAddr addr, state)
pointerToAddr state (DerefAddr addr) = (Addr addr, state)
pointerToAddr state (AddrAt addr i)  = (tmpAddr, newstate)
  where (tmpAddr, state1) = newTmp state
        newstate = addTAC state1 $ TACBinOp tmpAddr (Addr addr) "add" int_ i
pointerToAddr state (PointAddr addr) = (Addr addr, state)

pointerToAttrs :: State -> ElemAttrs -> (ElemAttrs, State)
pointerToAttrs state (ExprAttrs addr ty) = (ExprAttrs refAddr refTy, newstate)
  where (refAddr, newstate) | isAddressType ty = (addr, state)
                            | otherwise        = pointerToAddr state addr
        refTy | isAddressType ty = ty
              | otherwise        = pointerToType ty

--------------

accessAt :: State -> TACAddr -> TACAddr -> (TACAddr, State)
accessAt state (Addr a) i = (AddrAt a i, state)
accessAt state (AddrAt a i1@(ValAddr _)) i2@(ValAddr _) = (AddrAt a i, state)
  where i = intBOp "add" i1 i2
accessAt state (AddrAt a i1) i2 = (AddrAt a i, newstate)
  where (i, state1) = newTmp state
        newstate = addTAC state1 $ TACBinOp i i2 "add" int_ i1
accessAt state addr@(DerefAddr _) i = accessAt newstate tmpAddr i
  where (tmpAddr, state1) = newTmp state
        newstate = addTAC state1 $ TACAssign tmpAddr addr_ addr
accessAt state a1 a2 = error $ (show a1)++" "++(show a2)

accessAt0 :: State -> TACAddr -> (TACAddr, State)
accessAt0 state addr = accessAt state addr (ValAddr "0")

--------------

arrayAdvance :: State -> TACAddr -> Int -> (TACAddr, State)
arrayAdvance state addr@(AddrAt _ _) n = accessAt state addr $ ValAddr $ show n
arrayAdvance state a _ = error $ "Array advance on "++(show a)

--------------

localCopyAddr :: TACAddr -> TACAddr
localCopyAddr (Addr a) = Addr (a++"$copy")

------------------------------------------------------------------
-- Maybes:

maybeConvert :: State -> TACAddr -> Type -> Type -> (TACAddr, State)
maybeConvert state addr tyFrom tyTo | tyFrom == tyTo = (addr, state)
maybeConvert state (ValAddr c) (BasicType TypeChar) (BasicType TypeString)
  = (eAddr string, newstate)
  where (string, newstate) = addStaticString state (BString ((0,0), char))
        char = "\""++(init $ tail c)++"\""
maybeConvert state addr tyFrom tyTo = case (convertValAddr addr tyFrom tyTo) of
  ValAddr a -> (ValAddr a, state)
  NoAddr    -> (tmpAddr, state2)
  where (tmpAddr, state1) = newTmp state
        convop = "conv_"++(showAssignType tyFrom)++"_to_"
        state2 = addTAC state1 $ TACUnOp tmpAddr convop tyTo addr

maybeGetSimpleAddr :: State -> TACAddr -> Type -> (TACAddr, State)
maybeGetSimpleAddr state addr@(Addr a) ty = (addr, state)
maybeGetSimpleAddr state addr ty = getSimpleAddr state addr ty


------------------------------------------------------------------
-- Types:

getTACType :: State -> Type -> Type
getTACType state ty = case ty of
  PointerType t -> PointerType $ getTACType state t
  ArrayDimType _ elTy _ nExpr -> Array tacElTy n
    where (ExprAttrs nAddr nTy) = fst $ genExpr state nExpr
          n = getIntValue $ fst $ maybeConvert state nAddr nTy int_
          tacElTy = getTACType state elTy
  ArrayType _ _ _ -> error "ArrayType without dim."
  otherwise -> ty

getParamTACType :: State -> Param -> Param
getParamTACType state (Parameter x m t) = Parameter x m tacTy
  where tacTy = getTACType state t

getCompleteType :: State -> Type -> Expr -> Type
getCompleteType state ty expr = substitute basic (eTy $ fst $ genExpr state expr)
  where substitute t (BasicType _) = t
        substitute t (PointerType t1) = PointerType $ substitute t t1
        substitute t (Array t1 n) = Array (substitute t t1) n
        basic = getBasicType ty


-----------------------------------------------------------------
-- Expressions:

emptyRes :: ElemAttrs
emptyRes = ExprAttrs NoAddr ErrorType

addStaticString :: State -> BString -> (ElemAttrs, State)
addStaticString state bS@(BString ((_,_), s)) = (elem, newstate)
  where elem = ExprAttrs ptr string_
        (ptr, newstate) = case Map.lookup s (static state) of
          Just addr -> (addr, state)
          Nothing   -> (addr, state1 { static = Map.insert s ptr (static state)})
            where (addr, state1) = newStringAddress state


--------------------------------------------------------------------
-- Environment:

emptyEnv = Env (Map.empty) Nothing
initEnv = Env (Map.fromList builtInFuncList) Nothing

builtInFuncList :: [(Id, ElemAttrs)]
builtInFuncList
  = [("writeInt", (FuncAttrs (builtIn "writeInt")
         [Parameter (BIdent((0,0),"valInt")) ModVal int_]) void_),
     ("writeReal", (FuncAttrs (builtIn "writeReal")
         [Parameter (BIdent((0,0),"valReal")) ModVal real_]) void_),
     ("writeChar", (FuncAttrs (builtIn "writeChar")
         [Parameter (BIdent((0,0),"valChar")) ModVal char_]) void_),
     ("writeString", (FuncAttrs (builtIn "writeString")
         [Parameter (BIdent((0,0),"valString")) ModVal string_]) void_),
     ("readInt", (FuncAttrs (builtIn "readInt") [] int_)),
     ("readReal", (FuncAttrs (builtIn "readReal") [] real_)),
     ("readChar", (FuncAttrs (builtIn "readChar") [] char_)),
     ("readString", (FuncAttrs (builtIn "readString") [] string_))]


pushEnv :: State -> Env -> State
pushEnv state newenv = state { env = newenv }

pushStmtsEnv :: State -> [Stmt] -> State
pushStmtsEnv state stmts = addFuncsFromStmts newEnvState stmts
  where newEnvState = state { env = emptyEnv { parent = Just (env state) } }

popEnv :: State -> State
popEnv state = state { env = oldenv }
  where oldenv = getParentEnv $ env state

getParentEnv :: Env -> Env
getParentEnv env = trust $ parent env

findSymbol :: State -> Id -> ElemAttrs
findSymbol state name = findElem (env state) name
  where findElem e id = case Map.lookup id (names e) of
          Just a  -> a
          Nothing -> findElem (getParentEnv e) id

addSymbol :: State -> (Id, ElemAttrs) -> State
addSymbol state s = state { env = addSymbolToEnv (env state) s }

addSymbolToEnv :: Env -> (Id, ElemAttrs) -> Env
addSymbolToEnv env (id, elem)
  = env {names = Map.insert id elem (names env)}

isDeclared :: State -> Id -> Bool
isDeclared state id = Map.member id (names $ env state)


--------------------------------------------------------------------
-- State:

initState = State
  0               -- counterTmp
  0               -- counterLabel
  0               -- counterString
  initEnv         -- env
  NoLab           -- next
  (NoLab,NoLab)   -- ttff
  (NoLab,NoLab)   -- bbcc
  NoLab           -- returnLab
  default_handler -- onException
  initCode        -- tac
  []              -- functions
  initStatic      -- static

initCode = []
initStatic = Map.empty

default_handler = Label "default_handler"

newTmp :: State -> (TACAddr, State)
newTmp state = (addr, state {counterTmp = (ct + 1) })
  where ct   = counterTmp state
        addr = Addr ("t"++(show ct))
newLabel :: State -> (TACLabel, State)
newLabel state = (label, state { counterLabel = (lab + 1) })
  where lab   = counterLabel state
        label = Label ("l"++(show lab))

newStringAddress :: State -> (TACAddr, State)
newStringAddress state = (str, state { counterString = (cs + 1) })
  where cs  = counterString state
        str = ValAddr ("str"++(show cs))

inheritState :: State -> State -> State
inheritState dst src = dst { counterTmp    = (counterTmp src),
                             counterLabel  = (counterLabel src),
                             counterString = (counterString src),
                             static        = (static src) }

inheritStatic :: State -> State -> State
inheritStatic dst src = dst { counterString = (counterString src),
                              static = (static src) }

setNext :: State -> TACLabel -> State
setNext state nextlab = state { next = nextlab }
setTTFF :: State -> TACLabel -> TACLabel -> State
setTTFF state ttlab fflab = state { ttff = (ttlab, fflab) }
setBBCC :: State -> TACLabel -> TACLabel -> State
setBBCC state bblab cclab = state { bbcc = (bblab, cclab) }
setReturnLab :: State -> TACLabel -> State
setReturnLab state retLab = state { returnLab = retLab }
setOnException :: State -> TACLabel -> State
setOnException state excLab = state1 { onException = excLab }
  where state1 = addTAC state $ TACException excLab

isFallNext :: State -> Bool
isFallNext state = (next state) == Fall
isFallTT :: State -> Bool
isFallTT state = (fst $ ttff state) == Fall
isFallFF :: State -> Bool
isFallFF state = (snd $ ttff state) == Fall

gotoNext state = let Label _ = next state in
  addTAC state $ TACGoto (next state)
gotoTT state = case fst $ ttff state of
  Fall    -> state
  Label _ -> addTAC state $ TACGoto (fst $ ttff state)
gotoFF state = case snd $ ttff state of
  Fall    -> state
  Label _ -> addTAC state $ TACGoto (snd $ ttff state)
gotoBB state = let Label _ = (fst $ bbcc state) in
  addTAC state $ TACGoto (fst $ bbcc state)
gotoCC state = let Label _ = (snd $ bbcc state) in
  addTAC state $ TACGoto (snd $ bbcc state)
gotoReturn state = let Label _ = returnLab state in
  addTAC state $ TACGoto (returnLab state)
gotoOnException state = let Label _ = onException state in
  addTAC state $ TACGoto (onException state)

addTAC :: State -> TAC -> State
addTAC state code = state { tac = (code:tacs) }
  where tacs = tac state

addFunctionTAC :: State -> [TAC] -> State
addFunctionTAC state tac = state { functions = tac:fs }
  where fs = functions state

foldFuncs :: State -> State
foldFuncs state = addFuncs state $ reverse $ functions state
 where addFuncs s []     = s
       addFuncs s (f:fs) = addFuncs (s {tac = f++(tac s)}) fs

getStaticTAC :: State -> [TAC]
getStaticTAC state | null $ static state = []
                   | otherwise = TACStaticData:(map makeTAC $ Map.toList $ static state)
  where makeTAC (k, val) = TACStatic val k
