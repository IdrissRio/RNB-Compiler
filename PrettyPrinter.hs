{-# LANGUAGE FlexibleInstances#-}

module PrettyPrinter where

import Data.Map(Map,toList)
import AbsRnb
import TACGenerator
import Utils


errorMsg :: String -> String
errorMsg x  = (colorRed "[Error]: ")++ (colorWhite  x)

infoMsg :: String -> String
infoMsg x = (colorBlue "[Info]: ") ++ (colorWhite x)

warningMsg :: String -> String
warningMsg x = (colorYellow "[Warning]: ") ++ (colorWhite x)

colorRed :: String -> String
colorRed x  =  "\x1b[31;1m" ++ x ++ "\x1b[37;1m"

colorWhite :: String -> String
colorWhite x = "\x1b[37m" ++  x ++ "\x1b[37;1m"

colorGrey :: String -> String
colorGrey x = "\x1b[90;0m" ++"\x1b[90m"++  x ++ "\x1b[37;1m"

colorBlue :: String -> String
colorBlue x = "\x001b[34;1m" ++ x ++ "\x1b[37;1m"

colorCyan :: String -> String
colorCyan x = "\x001b[36m" ++ x ++ "\x1b[37;1m"

colorMagenta :: String -> String
colorMagenta x = "\x1b[35m" ++  x ++ "\x1b[37;1m"

colorYellow :: String -> String 
colorYellow x = "\x001b[33m" ++ x ++ "\x1b[37;1m"

colorGreen :: String -> String
colorGreen x = "\x1b[32;1m" ++ x ++ "\x1b[37;1m"

underlineMag :: String -> String
underlineMag x = "\x1b[35;4m" ++ x ++ "\x1b[37;0m"++"\x1b[37;1m"

underlineYel :: String -> String
underlineYel x = "\x001b[33;4m" ++ x ++ "\x1b[37;0m"++"\x1b[37;1m"

lineAndColumn :: Int -> Int -> String
lineAndColumn l c = " (line: " ++ (show l) ++", column: "++ (show c) ++")"

underlineError :: String -> String
underlineError x = "\x1b[34;1m" ++ x ++ "\x1b[37;1m"


colorDecl x = colorGreen x 
colorExpr x = colorBlue x 
colorStmt x = colorRed x
colorPos x = colorYellow x
colorMod x = colorCyan x 
colorRel x = colorMagenta x
colorLE x = colorMagenta x

------------------------------------------------------
-- PRINTS:
------------------------------------------------------

showId :: BIdent -> String 
showId (BIdent ((x,y),z)) = 
  "'"++ z ++ "', " ++ 
  colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")

showVal :: Val -> String
showVal v =
 case v of
 IntVal (BInteger ((x,y),z)) ->
  "Int  "++ z ++ ", " ++ 
  colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 RealVal (BReal ((x,y),z)) ->
  "Real "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 CharVal (BChar ((x,y),z)) ->
  "Char "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 BoolVal (BBool ((x,y),z)) ->
  "Bool "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 StringVal (BString ((x,y),z)) ->
  "String "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")

showTACAddr :: TACAddr -> (String,Int)
showTACAddr a = case a of
  Addr name -> colorName name
  PointAddr name -> colorName ('&':name)
  DerefAddr name -> colorName ('*':name)
  AddrAt name i -> (id++"["++ addr ++"]",dim1+dim2+2)
   where (id,dim1) = colorName name
         (addr,dim2) = showTACAddr i 
  ValAddr name -> colorName name
  NoAddr -> ("",0)

colorName :: String -> (String,Int)
colorName name = case elem '@' name of
  True -> fin
   where fin1 = takeWhile (/= '@') name
         dim = length fin1
         fin2 = drop (dim+1) name
         fin = (colorBlue fin1 ++ colorCyan "@" ++ colorBlue fin2,38)
  False -> (name,-2) 


showTACLabel :: TACLabel -> (String,Int)
showTACLabel lab = case lab of
  Label s -> colorLabel s 
  Fall    -> ("FALL",0)
  NoLab   -> ("",0)


colorLabel :: String -> (String,Int)
colorLabel lab = case elem '@' lab of
  True -> fin
   where fin1 = takeWhile (/= '@') lab 
         dim = length fin1
         fin2 = drop (dim+1) lab
         fin = (colorBlue fin1 ++ colorCyan "@" ++ colorBlue fin2,40)
  false -> (lab, -2) 

------------------------------------------------------
-- PRETTY TAC:
------------------------------------------------------

padLabel = 4
padVar = 3
tiny = 6

padTAC x n
 | n > 0 = padTAC (" " ++ x ) (n-1)
 | n == 0 = x

frameline = "############"

prettyTAC ::  [TAC] -> String
prettyTAC a =  buildTACS a (setBigSmall a (0,0) )

setBigSmall :: [TAC] -> (Int,Int) -> (Int,Int)
setBigSmall [] (big,small) = (big+padLabel,small+padVar)
setBigSmall (x:xs) (big,small) = case x of
  TACAssign  tacAddr _ _ -> setBigSmall xs (big,newSmall)
    where (s,dim) = showTACAddr tacAddr
          newSmall = max small (length s - dim ) 
  TACUnOp tacAddr _ _ _ -> setBigSmall xs (big,newSmall)
    where (s,dim) = showTACAddr tacAddr
          newSmall = max small (length s - dim )      
  TACBinOp tacAddr _ _ _ _ -> setBigSmall xs (big,newSmall)
    where (s,dim) = showTACAddr tacAddr
          newSmall = max small (length s - dim ) 
  TACException _ -> setBigSmall xs (big,newSmall)
    where newSmall = max small 15                          
  TACAddLabel (Label s) ->
    setBigSmall xs ((max big (length s )),small)
  TACStatic tacAddr _ -> 
    setBigSmall xs ((max big (
      length (fst $ showTACAddr  tacAddr) )),small)
  TACReturn _ -> setBigSmall xs (big,(max small 6))
  otherwise  -> setBigSmall xs (big,small)

buildTACS :: [TAC] -> (Int,Int) -> String 
buildTACS [] _ = ""
buildTACS ((TACAddLabel l1):(TACAddLabel l2):xs) y 
  = " " ++lab ++ ":\n" ++ buildTACS ((TACAddLabel l2):xs) y
   where (lab1,dim) = showTACLabel l1
         lab = case dim <= 0 of
           True -> lab1
           otherwise -> "\n "++lab1
buildTACS ((TACAddLabel tacLabel):t:xs) (big,small)
 = " "++lab ++ ":"
   ++ padTAC (buildTAC t small) (big - length lab +dim)
   ++ buildTACS xs (big,small)
    where (lab1,dim) = showTACLabel tacLabel
          lab = case dim <=0 of
            True -> lab1
            otherwise -> "\n "++lab1
buildTACS (TACCode: xs) y 
 = colorGrey("\n"++frameline ++ "    TAC Code    "++frameline++"\n\n")
  ++ buildTACS xs y
buildTACS (TACStaticData:xs) y 
 = colorGrey ("\n" ++ frameline++"   Static Data  "++frameline++"\n\n")
   ++ buildTACS xs y 
buildTACS ((TACStatic tacAddr string):xs) (big,small) 
 = " " ++ addr ++ ":" 
   ++ padTAC string (big - length addr + dim ) ++ "\n"
   ++ buildTACS xs (big,small)
     where (addr,dim) = showTACAddr tacAddr
buildTACS (x:xs) (big,small)  =
   padTAC (buildTAC x small  ) big  ++ buildTACS xs (big,small)

buildTAC :: TAC -> Int -> String  
buildTAC t small = case t of
  TACAssign tacAddr1 type_ tacAddr2
    -> leftAddr ++ padTAC "=" (small - length leftAddr + dim) ++
        (underlineMag assignType) ++
         padTAC (rightAddr)  (tiny - length assignType) ++ "\n"
        where (leftAddr,dim) = showTACAddr tacAddr1
              assignType = showAssignType type_
              (rightAddr,_) = showTACAddr tacAddr2
  TACUnOp tacAddr1 tacOperator type_ tacAddr2
    -> leftAddr ++ padTAC  "=" (small - length leftAddr + dim ) 
       ++(underlineMag (assignType ))  ++
       padTAC   (colorYellow(tacOperator2 ++ assignType)) (tiny - length assignType)
       ++  " "++ rightAddr  ++ "\n"
        where (leftAddr,dim) = showTACAddr tacAddr1
              (rightAddr,_)  = showTACAddr tacAddr2
              assignType = showAssignType type_
              tacOperator2 = case elem 'c' tacOperator of
                True -> tacOperator
                False -> tacOperator ++"_"
  TACBinOp tacAddr1 tacAddr2 tacOperator type_ tacAddr3
    -> leftAddr++padTAC  "=" (small - length leftAddr + dim)
       ++ (underlineMag assignType )
       ++ padTAC (rightAddr1) (tiny -length assignType)  ++ " "
       ++ colorYellow (tacOperator++  "_" ++assignType) 
       ++" "++ rightAddr2++ "\n"
         where (leftAddr,dim) = showTACAddr tacAddr1
               (rightAddr1,_) = showTACAddr tacAddr2
               (rightAddr2,_) = showTACAddr tacAddr3
               assignType = showAssignType type_
  TACGoto tacLabel
    -> "goto " ++lab++ "\n"
      where (lab,_) = showTACLabel tacLabel 
  TACIf tacAddr tacLabel
    -> "if "++ addr ++" goto "
       ++ lab ++ "\n"
      where (addr,_) = showTACAddr tacAddr
            (lab,_) = showTACLabel tacLabel 
  TACIfFalse tacAddr tacLabel
    -> "ifFalse "++addr ++" goto "
        ++lab ++ "\n"
        where (addr,_) = showTACAddr tacAddr
              (lab,_) = showTACLabel tacLabel 
  TACIfRel tacAddr1 rel ty tacAddr2 tacLabel
    -> "ifRel (" ++addr1 ++" "++
       colorYellow (showTACRel rel ++ "_")
       ++(colorYellow (showAssignType ty))++" " ++ addr2 ++
       ") goto " ++lab ++ "\n"
      where (addr1,_) = showTACAddr tacAddr1
            (addr2,_) = showTACAddr tacAddr2
            (lab,_) = showTACLabel tacLabel
  TACParam tacAddr
    -> (underlineMag "param")++" " ++addr ++ "\n"
      where (addr,_) = showTACAddr tacAddr
  TACCallProc tacLabel
    -> (underlineMag "call")++" "++  lab   ++ "\n"
      where (lab,_) = showTACLabel tacLabel 
  TACCallFunc tacAddr ty tacLabel
    -> leftAddr ++ padTAC "=" (small - length leftAddr+dim)
       ++ (underlineMag assignType )
       ++ padTAC (underlineMag "call") (tiny - length assignType) ++ " "++ lab ++ "\n"
         where (leftAddr,dim) = showTACAddr tacAddr
               (lab,_) = showTACLabel tacLabel
               assignType = showAssignType ty
  TACReturn tacAddr
    -> (underlineMag"return") ++" "++ addr ++ "\n"
      where (addr,_) = showTACAddr tacAddr
  TACComment comment
    -> colorGrey("// "++ comment) ++ "\n"
  TACExit -> "exit \n"
  TACAddLabel tacLabel  -> "\n " ++ label ++"\n"
    where (label,_) = showTACLabel tacLabel
  TACException tacLabel -> colorRed "onexceptiongoto " ++ label ++"\n"
    where (label,_) = showTACLabel tacLabel



printState :: State -> String
printState (State tmp lab str env next (tt,ff) (bb,cc) _ _ _ _ _)
  = "State \n tmp = " ++ show tmp ++
    "\n lab = " ++ show lab ++
    "\n str = " ++ show str ++
    "\n env = " ++ showEnv env ++
    "\n next " ++  next1 ++
    "\n tt ff " ++" ("  ++
       tt1 ++","++  ff1 ++ ")"++
    "\n break continue " ++" ("  ++
       bb1 ++","++  cc1 ++ ")"
      where (next1,_) = showTACLabel next
            (tt1,_) = showTACLabel tt
            (ff1,_) = showTACLabel ff
            (bb1,_) = showTACLabel bb
            (cc1,_) = showTACLabel cc
      
showEnv :: Env -> String
showEnv (Env symbols Nothing) =  (showNameMap symbols)
showEnv (Env symbols (Just env)) = (showNameMap symbols) ++
  "\n\t parent: " ++ (showEnv env)

showNameMap :: Map Id ElemAttrs -> String
showNameMap m
  | null m = ""
  | otherwise = showName (toList m)


showName :: [(Id,ElemAttrs)] -> String
showName [] = ""
showName ((id, attr):ss) = "\n\t" ++ id ++
  "\t== " ++ (showAttr attr) ++ (showName ss)

showAttr :: ElemAttrs -> String
showAttr (ExprAttrs addr ty)
   = "{ LExpr  " ++ addr1 ++
     "  :: " ++ showType ty
   where (addr1,_) = showTACAddr addr
showAttr (FuncAttrs callat params ty)
   = "{  callat = " ++ callat1 ++
     "  retty = " ++ show ty ++
     "  params = " ++ show params
  where (callat1,_) = showTACLabel callat



------------------------------------------------------
-- PRETTY TREE:
------------------------------------------------------

pad first rest = zipWith (++) (first : repeat rest)

paddingMultiple  x = ((pad "`- " "| ") x)
paddingSingle x = ((pad "`- " "  ") x)

prettyTree ::  Program -> String
prettyTree (Start decls) = unlines (buildTree decls) 


class PrintTree a where 
 buildTree :: a -> [String]
 
instance PrintTree [Decl] where 
 buildTree [] = [] 
 buildTree [d] = paddingSingle (buildTree d)
 buildTree (d:ds) = (paddingMultiple (buildTree d)) ++ buildTree ds 
 
 
instance PrintTree Decl where
 buildTree x =
  case x of 
  VarDecl id type_ -> 
   (colorDecl "VarDecl " ++ showId id ++", " ++ showType type_):[] 
  VarDef id type_ _ expr ->  
    (colorDecl "VarDef " ++ showId id++", " ++ showType type_)
      :  (buildTree [expr])
  ConstDef id type_ _ expr -> 
    (colorDecl "ConstDef " ++ showId id++", " ++ showType type_)
      :  (buildTree [expr])
  FuncDef id1 param id2 type_ [] _ -> 
    (colorDecl "FuncDef " ++ showId id1) 
    : ( buildTree param
      ++ (paddingSingle((showId id2++", " ++ showType type_):[])))
  FuncDef id1 param id2 type_ (stmt:s) _ -> 
    (colorDecl "FuncDef " ++ showId id1) 
    : ( buildTree param
      ++ (paddingMultiple((showId id2++", " ++ showType type_):[]))
      ++ (buildTree (stmt:s)))
  ProcDef id param [] id2 ->
    (colorDecl "ProcDef " ++ showId id)
    : ( (buildTree param )
      ++ (paddingSingle ((showId id2 ):[])))
  ProcDef id param (stmt:s) _ ->
    (colorDecl "ProcDef" ++ showId id)
    : (( (buildTree param ))
      ++ (buildTree (stmt:s)))

instance PrintTree [Param] where 
 buildTree [] = [] 
 buildTree [d] = paddingMultiple (buildTree d)
 buildTree (d:ds) = (paddingMultiple (buildTree d)) ++ buildTree ds 

instance PrintTree Param where 
 buildTree  (Parameter id mod type_ ) = 
  (colorMod (show mod)) : ( paddingSingle
   (((showId id) ++", "++ (showType type_)) :[]))

instance PrintTree [Expr] where
 buildTree [] = []
 buildTree [e] = paddingSingle (buildTree e) 
 buildTree (e:es) = (paddingMultiple (buildTree e)) ++ buildTree es 
  
instance PrintTree Expr where
 buildTree e = 
  case e of  
  ExprVal val ->
   ((colorExpr "ExprVal ") ++ (showVal val)):[]
  ExprLeft leftExpr ->  
   (((colorExpr "ExprLeft ")): (paddingSingle (buildTree leftExpr)))
  ExprAddress leftExpr ->  
   (((colorExpr "ExprAdress ")): (paddingSingle (buildTree leftExpr)))
  ExprCall id expr -> 
   (((colorExpr "ExprCall ") ++ showId id) : buildTree expr )
  ExprArray _ expr -> 
   (((colorExpr "ExprArray ")) : buildTree expr )
  ExprCreate _ type_ _ expr ->
   (((colorExpr "ExprCreate ")++"Array <"++ (showType type_)++"> ") 
   : ((buildTree [expr])))
  ExprBrack expr ->
   ((colorExpr "ExprBrack ") : ((buildTree [expr])))
  ExprMul expr1 _ expr2 ->
   ((colorExpr "ExprMul ") 
   : (paddingMultiple(buildTree expr1) ++ buildTree [expr2]))
  ExprPow expr1 _ expr2 ->
   ((colorExpr "ExprPow ") 
   : (paddingMultiple(buildTree expr1) ++ buildTree [expr2] ))
  ExprMod expr1 _ expr2 ->  
   ((colorExpr "ExprMod ") 
   : (paddingMultiple(buildTree expr1) ++ buildTree [expr2] ))
  ExprDiv expr1 _ expr2 ->
   ((colorExpr "ExprDiv ") 
   : (paddingMultiple(buildTree expr1) ++ buildTree [expr2] ))
  ExprSum expr1 _ expr2 ->
   ((colorExpr "ExprSum ")
   : (paddingMultiple (buildTree expr1) ++ buildTree [expr2] ))
  ExprUnaryMinus _ expr ->
   ((colorExpr "ExprUnaryMinus ") : (buildTree [expr] ))
  ExprUnaryPlus _ expr ->
   ((colorExpr "ExprUnaryPlus ") : (buildTree [expr] ))
  ExprMinus expr1 _ expr2 ->
   ((colorExpr "ExprMinus ") 
   : (paddingMultiple (buildTree expr1)  ++ buildTree [expr2] ))
  ExprPostIncr leftExpr ->
   ((colorExpr "ExprPostIncr") : (paddingSingle(buildTree leftExpr )))
  ExprPostDecr leftExpr ->
   ((colorExpr "ExprPostDecr") : (paddingSingle(buildTree leftExpr )))
  ExprAnd expr1 _ expr2 ->
   ((colorExpr "ExprAnd ") 
   : (paddingMultiple(buildTree expr1) ++ buildTree [expr2] ))
  ExprOr expr1 _ expr2 ->
   ((colorExpr "ExprOr ") 
   : (paddingMultiple (buildTree expr1)  ++ buildTree [expr2] ))
  ExprNot _ expr ->  ((colorExpr "ExprNot") : (buildTree [expr] ))
  ExprRel expr1 rel expr2 ->
   ((colorExpr "ExprRel") 
   : (paddingMultiple (buildTree expr1) ++((buildTree rel)
     ++ (buildTree [expr2] ))))
  ExprIf expr1 expr2 expr3 -> ((colorExpr "ExprIf")) :
   (paddingMultiple (buildTree expr1) ++ paddingMultiple (buildTree expr2) ++
    paddingSingle (buildTree expr3))

instance PrintTree Rel where
  buildTree x =case x of
    RelLess _ -> paddingSingle ("RelLess":[])
    RelLessEq _ -> paddingSingle ("RelLessEq":[])
    RelGreater _ -> paddingSingle ("RelGrater":[])
    RelGreaterEq _ -> paddingSingle ("RelGraterEq":[])
    RelNotEq _ -> paddingSingle ("RelNotEq":[])
    RelEq _ -> paddingSingle ("RelEq":[])
    
 
 
instance PrintTree LeftExpr where
 buildTree e =
  case e of 
  LEId id -> ((colorLE "LEId ") ++ (showId id)):[]
  LEDeref leftExpr -> 
   (((colorLE "LEDeref ")) :(paddingSingle ( buildTree leftExpr)))
  LEArray leftExpr dim ->
   (((colorLE "LEArray ")) 
   : (paddingMultiple (buildTree leftExpr) ++ buildTree dim)) 
  LEBra leftExpr -> 
   ((colorLE "LEBra ") : (paddingSingle (buildTree leftExpr)))
  LEPreIncrem leftExpr -> 
   ((colorLE "LEPreIncrem ") : (paddingSingle (buildTree leftExpr)))
  LEPreDecr leftExpr -> 
   ((colorLE "LEPreDecr ") : (paddingSingle (buildTree leftExpr)))
  
instance PrintTree Dim where 
 buildTree d = 
  case d of 
  DimArray _ expr ->  
   paddingSingle(("DimArray") : (paddingSingle  (buildTree expr))) 

instance PrintTree [[Stmt]] where
 buildTree [[]] = []
 buildTree ((x:xs):xss) =(paddingMultiple (buildTree x)) ++ buildTree (xs:xss)

instance PrintTree [Stmt] where
 buildTree [] = []
 buildTree [s] = paddingSingle (buildTree s)
 buildTree (s:ss) = ( paddingMultiple (buildTree s)) ++ buildTree ss 

instance PrintTree Stmt where
 buildTree x =
  case x of
  StmtAssign leftExpr _ expr ->
   ((colorStmt "StmtAssign " ) 
   : ((paddingMultiple (buildTree leftExpr)) ++ buildTree [expr]))
  StmtIf expr stmt elseStmt -> 
   ((colorStmt "StmtIf " ) 
   : ((paddingMultiple ( buildTree expr)) 
   ++ (buildTree [stmt]) ++ (buildTree elseStmt))) 
  StmtLoop _ stmt _ -> ((colorStmt "StmtLoop " ) : (buildTree stmt))  
  StmtCase _ expr cases _ ->
   ((colorStmt "StmtCase " ) 
   : ((paddingMultiple (buildTree expr)) ++ buildTree cases))  
  StmtLoopCmd loopCmd -> 
   ((colorStmt "StmtLoopCmd " ):(paddingSingle (buildTree loopCmd) ))  
  StmtReturn _ -> ((colorStmt "StmtReturn " )) : []  
  StmtLocal stmt -> ((colorStmt "StmtLocal " ) : (buildTree stmt))
  StmtDecl decl -> ((colorStmt "StmtDecl " ) : (buildTree [decl]))
  StmtExpr expr -> ((colorStmt "StmtExpr " ) : (buildTree [expr]))
  StmtFor bident btype bass expr1 dir expr2 stmts -> (colorStmt "StmtFor") :
   (paddingMultiple(buildTree( VarDef  bident  (BasicType btype) bass expr1 ))++ 
    paddingSingle ([(show dir)]) ++ padExpr ++ buildTree stmts)
    where 
    padExpr = case stmts of 
        [] -> paddingSingle ( buildTree expr2) 
        otherwise  -> paddingMultiple(buildTree expr2)
  StmtTryCatch bTry stmts1 bcatch1 stmts2 bcatch2 -> (colorStmt "StmtTryCatch") :(  
   paddingMultiple ( colorStmt "BTry" : (buildTree stmts1)) ++ 
   paddingSingle ( colorStmt "Bcathc" : (buildTree stmts2) ))


instance PrintTree LoopCmd where 
 buildTree x = 
  case x of 
  LoopExitOn bExitOn expr  -> 
   ((colorStmt "LoopExitOn")) :  (buildTree [expr])
  LoopExit bExit -> 
   ((colorStmt "LoopExit")) :[]
  LoopContinue bContinue -> 
   ((colorStmt "LoopContinue")) :[]

instance PrintTree ElseStmt where
 buildTree x =
  case x of 
  StmtElseIf expr stmt elseStmt -> 
   paddingSingle((colorStmt "StmtElseIf") 
   : ((paddingMultiple (buildTree expr)
   ++ (buildTree [stmt]))) ++ (buildTree elseStmt))
  StmtElse stmt -> 
   paddingSingle ((colorStmt "StmtElse") : ((buildTree stmt)))
  StmtNoElse -> paddingSingle ((colorStmt "StmtNoElse") :[])

instance PrintTree Cases where 
 buildTree c = 
  case c of
  CaseEps -> paddingSingle ("CaseEps" : []) 
  CaseDefault stmt ->  
   paddingSingle ("CaseDefault"  :  (buildTree stmt))
  CaseVal expr stmt cases -> 
   paddingMultiple ("CaseVal" 
   : (((paddingMultiple (buildTree expr)) 
      ++ (buildTree stmt))))
      ++ (buildTree cases)

