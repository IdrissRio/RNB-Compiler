module ErrorStaticAnalysis where
import AbsRnb
import Utils
import PrettyPrinter


tabSpace :: String
tabSpace = "         "

errorType :: Error -> String
errorType x =
    case x of 
        RedeclaredVar (BIdent ((l,c),name)) -> 
            errorMsg "Symbol name '"++ name ++ "' for variable declaration already used. " 
            ++ (lineAndColumn l c) ++  " [DeclError]."
            
        RedeclaredFunction (BIdent ((l,c),name)) -> 
            errorMsg "Symbol name '"++ name ++ "' for function declaration already used. " 
            ++ (lineAndColumn l c) ++  " [DeclError]."

        RedeclaredParameter (BIdent ((l,c),name)) -> 
            errorMsg "Symbol name '"++ name ++ "' for parameter declaration already used. " 
            ++ (lineAndColumn l c) ++  " [DeclError]."
        
        TypeMismatch (l,c)  ty1 ty2 -> 
            errorMsg "Incompatible conversion from '"++ (showType ty1)++
            "' to '" ++ (showType ty2)++"'."++ (lineAndColumn l c)  ++  " [TypeError]."
        
        FunctionNameMismatch (l,c) nameBegin ->
            errorMsg "Maybe you meant '"++ nameBegin ++ "'. The ending name should be equal to the function identifier."++ (lineAndColumn l c)  ++  " [DeclError]."
        
        NonOmogeneusArray (l,c) ->
            errorMsg "Non homogeneous array initialization."++ (lineAndColumn l c)  ++  " [TypeError]."
    
        NonConstantExpr (l,c) ->
            errorMsg "Array dimension must be constant and known at compile time in array initialization."++ (lineAndColumn l c)  ++  " [DeclError]."
        
        NonIntegerExpr (l,c) ->
            errorMsg "Array dimension must be an integer value in array initialization."++ (lineAndColumn l c)  ++  " [TypeError]."
       
        NonIntegerExprInArrayAccess (l,c)->
            errorMsg "Array access requires an integer value."++ (lineAndColumn l c)  ++  " [TypeError]."

        NotDeclared (BIdent ((l,c),name)) ->
            errorMsg "Use of undeclared identifier '"
            ++ name ++ "'." ++ (lineAndColumn l c) ++  " [DeclError]."
            
        DereferenceExcess (BIdent ((l,c),_)) -> 
            errorMsg "Dereferencing requires pointer operand."  
            ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NotAnArray (l,c) -> 
            errorMsg "Operator [] requires an Array."  
            ++ (lineAndColumn l c) ++  " [TypeError]."
            
        PointerArithmetic (l,c) -> 
            errorMsg "Pointer arithmetic is not allowed in RNB."  
            ++ (lineAndColumn l c) ++  " [TypeError]."
            
        NonNumericValue (l,c)  -> 
            errorMsg "Required a numerical value."
            ++ (lineAndColumn l c) ++  " [TypeError]."
            
        DifferentLenght (l,c) -> 
            errorMsg "All initialization list elements must have the same lenght."
            ++ (lineAndColumn l c) ++  " [DeclError]."
            
        EmptyList (l,c) -> 
            errorMsg "Initialization lists must have at least one element."
            ++ (lineAndColumn l c) ++  " [DeclError]."
            
        NegativeSize (l,c) -> 
            errorMsg "Array size requires a positive integer."
            ++ (lineAndColumn l c) ++  " [TypeError]."

        NegativeSizeIndex (l,c) -> 
            errorMsg "Array index must be a positive integer."
            ++ (lineAndColumn l c) ++  " [TypeError]."

        DivideByZero (l,c) -> 
            errorMsg "Division by zero."
            ++ (lineAndColumn l c) ++  " [ArithmeticError]."
            
        NonBasicType (l,c) typ-> 
            errorMsg "Expected a basic type but found '" ++ showType typ ++ "'. \n"++tabSpace ++ "E.g., 'int', 'real', 'string', 'char' and 'bool'."
            ++ (lineAndColumn l c) ++  " [DeclError]."
        
        ArithmeticError ty1 ty2 (l,c) op 
            | op == "^" || op == "%" -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Int'.\n"++tabSpace ++ "Maybe you want to use '"++op++": int x int -> int'" ++ (lineAndColumn l c) ++  " [TypeError]."
            | op == "/" -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> τ'.\n"++tabSpace ++ "Maybe you want to use '"++op++": τ_1 x τ_2 -> real'.\n" ++tabSpace ++"Where τ_1, τ_2 ∈ {'int','real'}.\n"++tabSpace++"More details can be found in the documentation." ++ (lineAndColumn l c) ++  " [TypeError]."
            | otherwise -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> τ'.\n"++tabSpace ++ "Maybe you want to use '"++op++": τ_1 x τ_2 -> τ3'.\n" ++tabSpace ++"Where τ_1, τ_2 ∈ {'int','real'}  and τ_3 is the max between τ_1 and τ_2.\n"++tabSpace++"More details can be found in the documentation." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        RelError (l,c) ty1 ty2 rel -> 
            errorMsg "Undefined operator '"++ showRel rel ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Bool'."++{-\n++tabSpace ++ "Expected 2 numerical values, e.g., 'int' or 'real'."++ -}(lineAndColumn l c) ++  " [TypeError]."
            
        BoolOpError (l,c) ty1 ty2 op -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Bool'.\n"++tabSpace ++ "Expected 2 boolean values."++ (lineAndColumn l c) ++  " [TypeError]."
        
        BoolNotError (l,c) ty1 op -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" -> Bool'.\n"++tabSpace ++ "Expected a boolean value."++ (lineAndColumn l c) ++  " [TypeError]."
        
        FuncArgsMismatch exprs params (BIdent ((l,c),_)) ->
            errorMsg "Mismatch in number of arguments. "++ (lineAndColumn l c) ++  " [TypeError].\n"++ tabSpace ++"The call function has "++ show( length exprs) ++" arguments while " ++ show (length params)++ " are required." 
            
        FuncTypesMismatch exprs params (BIdent ((l,c),name)) (ld, cd)->
            errorMsg "Mismatch in argument types."++ (lineAndColumn l c) ++  " [TypeError].\n" ++ tabSpace ++ "In function call: '" ++ (showFunction ( name ++ "(" ) exprs)++ "'.\n"++tabSpace ++
                "Function definition found at <l:" ++ (show ld) ++ ",c:" ++ (show cd) ++ "> with signature: '" ++ ( showFunction (name++"(") params) ++
                "'."
        AssignConstToVar (l,c) ->
            errorMsg "You cannot assign the address of a constant variable to a variable."  
                ++ (lineAndColumn l c) ++  " [TypeError]."

        PassingRefOfConst (l,c) ->
            errorMsg "Passing the address of a const qualified variable."  
                ++ (lineAndColumn l c) ++  " [TypeError]."
        
        ChangingConstVar (BIdent ((l,c),name)) ->
            errorMsg "Cannot change the value of the const variable '"++ name ++ "'."
            ++ (lineAndColumn l c) ++  " [TypeError]."
            
        NotInLoop (l,c) s ->
            errorMsg  "'"++s ++"' statement not in a loop statement."
            ++ (lineAndColumn l c) ++  " [ContextError]."
        
        NonBooleanExpr (l,c)  ty-> 
            errorMsg "Expected a boolean expression but found '" ++ showType ty  ++ "'."
            ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NonConstPass (l,c) parNum (BIdent (_,name)) -> errorMsg "Cannot pass a const variable if the formal parameter is  not 'const' qualified.\n"++ tabSpace ++ "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' is not const qualified." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NonRefLeft (l,c) parNum (BIdent (_,name)) -> errorMsg "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' requires a Left Expression." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NonEqRefTypes (l,c) parNum (BIdent (_,name)) typReq typFound -> errorMsg "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' requires exactly the type '" ++ showType typReq++ "' but found '" ++ showType typFound ++"'." ++ (lineAndColumn l c) ++  " [TypeError]."

        NonCompExpr baseType exprType (l,c) -> errorMsg "Non compatible types in 'case' statement. Expected '" ++ showType baseType  ++ "' but found '" ++ showType exprType ++ "'." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        CaseExprNotEq (l,c) ty->errorMsg "Illegal expression of type '" ++ showType ty ++ "' inside the statement 'case'.\n"++
            tabSpace ++ "The operator '= :"++showType ty++ " x "++ showType ty ++ " -> bool' is not defined." 
            ++ (lineAndColumn l c) ++  " [TypeError]."

        ErrorDeclaration (BIdent ((l,c),_)) kind -> errorMsg kind++" declaration must be also definition."
            ++ (lineAndColumn l c) ++  " [DeclError]."

        FunctionWithoutDimension (BIdent ((l,c),name)) -> errorMsg "The array dimension for the parameter '"++ name ++ "' must be specified."
            ++ (lineAndColumn l c) ++  " [TypeError]."

        NotAnArrayForEach (l,c) -> errorMsg "The 'foreach' statement requires an array type."++ (lineAndColumn l c) ++  " [TypeError]."

        NotIntegerType (l,c) ty -> errorMsg "Expected an integer value but found '" ++ showType ty ++"'." ++ (lineAndColumn l c) ++  " [TypeError]."

        NotCompileTime (l,c) -> errorMsg "The expression must be evaluable at compile time. "++ (lineAndColumn l c) ++  " [TypeError]."

        IncompatibleExprs (l,c) ty1 ty2 -> errorMsg "The two types '"++ showType ty1 ++ "' and '"++ showType ty2 ++ "''  in '("++ showType ty1 ++ ") if (·) else ("++  showType ty2 ++")' must coincide."++ (lineAndColumn l c) ++  " [TypeError]."

        FunctionAsIdentifier (l,c) -> errorMsg "Cannot use a function as a variable."++ (lineAndColumn l c) ++  " [TypeError]."

        ReturnInForLoop (l,c) stmt-> errorMsg "Cannot use the statement '"++ stmt++ "' inside a deterministic 'for' loop. " ++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]."

-------------------------------------------------
------------------ WARNINGS ---------------------
-------------------------------------------------

        WarEmptyCase (l,c) -> warningMsg "Empty 'case' statement." ++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]."

        UnsusedValue (l,c) -> warningMsg "Expression result unused." ++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]."  

        UnitializedVariable (BIdent ((l,c),name)) -> warningMsg "The return variable '"++ name ++"' may be never initialized."++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]." 

        DeathCode (l,c) -> warningMsg "Dead code after 'return' statement." ++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]."
        
        NoMain (l,c) ->warningMsg "Declaration of 'main' not found."

        InfiniteLoop (l,c) -> warningMsg "Infinite loop." ++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]."


        
data Error = RedeclaredVar BIdent |
             RedeclaredFunction BIdent |
             RedeclaredParameter BIdent |
             TypeMismatch Pos Type Type |
             FunctionNameMismatch Pos Id |
             NonOmogeneusArray Pos |
             NonConstantExpr Pos |
             NonIntegerExpr Pos |
             NonIntegerExprInArrayAccess Pos |
             NotDeclared BIdent |
             DereferenceExcess BIdent |
             NotAnArray Pos |
             PointerArithmetic Pos |
             NonNumericValue Pos |
             ArithmeticError Type Type Pos String| 
             DifferentLenght Pos |
             EmptyList Pos |
             NegativeSize Pos |
             NegativeSizeIndex Pos |
             DivideByZero Pos |
             NonBasicType Pos Type |
             RelError Pos Type Type Rel |
             BoolOpError Pos Type Type String |
             BoolNotError Pos Type String |
             FuncArgsMismatch [Type] [Type] BIdent |
             FuncTypesMismatch [Type] [Type] BIdent Pos |
             AssignConstToVar Pos |
             ChangingConstVar BIdent |
             NotInLoop Pos String |
             NonBooleanExpr Pos Type |
             NonConstPass Pos Int BIdent |
             WarEmptyCase Pos |
             NonCompExpr Type Type Pos|
             NonRefLeft Pos Int BIdent |
             NonEqRefTypes Pos Int BIdent Type Type|
             UnsusedValue Pos |
             PassingRefOfConst Pos |
             CaseExprNotEq Pos Type |
             ErrorDeclaration BIdent String |
             FunctionWithoutDimension BIdent |
             UnitializedVariable BIdent |
             DeathCode Pos|
             NoMain Pos |
             NotAnArrayForEach Pos |
             NotIntegerType Pos Type |
             NotCompileTime Pos |
             InfiniteLoop Pos |
             IncompatibleExprs Pos Type Type| 
             FunctionAsIdentifier Pos |
             ReturnInForLoop Pos String
             