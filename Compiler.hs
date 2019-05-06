module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Data.List

import LexRnb
import ParRnb
import SkelRnb

import AbsRnb
import PrettyPrinter
import StaticAnalysis
import TACGenerator
import Utils

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v < 1) $ putStrLn s

putStrC :: Verbosity -> String -> IO ()
putStrC v s = when (v == 6) $ putStrLn s

putStrN :: Verbosity -> String -> IO ()
putStrN v s = when (v < 3) $ putStrLn s 

putStrS :: Verbosity -> String -> IO ()
putStrS v s = when (v ==4 ) $ putStrLn s 

putStrT :: Verbosity -> String -> IO ()
putStrT v s = when (v == 5) $ putStrLn s 

putStrNC :: Verbosity -> String -> IO()
putStrNC v s = when (v==7) $ putStrLn s

compile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
compile v p f = putStrLn f >> readFile f >>= run v p

run ::  Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn (colorRed "[PARSING]: FAIL" )
                          putStrLn $ errorMsg s
                          exitFailure
           Ok  tree -> do putStrN v $ "\n"++(colorGreen "[PARSING]: PASS")
                          putStrNC v $ "\n[PARSING]: PASS"
                          putStrT v (testingTAC (generateTAC tree) s)
                          x <-testing tree s
                          putStrC v x
                          putStrV v $ typ
                          putStrN v $ err
                          putStrNC v $ removeAnsi typ
                          putStrNC v $ removeAnsi err
                          --putStrN v ( StaticAnalysis.printEnv ( rnbStaticAnalysis tree))
                          if(nErr==0) then
                             do
                             putStrS v $ (silentAnalysis)
                             putStrN v $ (infoMsg "TAC Generator\n")++(prettyTAC (generateTAC tree))
                             putStrNC v $ removeAnsi ((infoMsg "TAC Generator\n")++(prettyTAC (generateTAC tree)))
                             --putStrV v $ (infoMsg "TAC State\n")++(printState $ genProgram tree)
                            else
                              exitSuccess
                          exitSuccess
                          where (typ,err,silentAnalysis, nErr) = showTree tree s

-- Helpers

listPosition :: [String] -> [(String,Pos)] -> [(String, (String,Pos))]
listPosition _ [] = []
listPosition list (p:pos) =case snd p of
  (0,0) -> (list!!((fst(snd p))), p) : (listPosition list pos)
  otherwise -> (list!!((fst(snd p))-1), p) : (listPosition list pos)

printErrs errs = map printTypechekerError errs



printTypechekerError :: (String , (String , Pos)) -> String
printTypechekerError (code, (err, (_, col))) = case col == 0 of
    False ->
        err++"\n" ++
        code++"\n" ++
        underlineError ((replicate (col-1) '~' )++ "^")
    True -> err++"\n"

sortErr :: (String,(String,Pos)) -> (String,(String,Pos))-> Ordering
sortErr (_,(_,p1)) (_,(_,p2)) = sortPos p1 p2
  where sortPos (lin,col) (lin2,col2) 
          | lin < lin2 = LT
          | lin > lin2 = GT
          | lin == lin2 && col < col2 = LT
          | otherwise = GT

countError ::   [(String,Pos)] -> Int
countError [] = 0
countError ((x,_):xs) = case isInfixOf "[Error]:" x of
  True -> 1 + countError xs
  False -> countError xs


showTree :: Program -> String -> (String,String,String,Int)
showTree  tree s= (("\n"++infoMsg ("Abstract Syntax Tree\n") ++
                   prettyTree tree) ++"\n", unlines (printErrs (sortBy sortErr (listPosition listTree envErrorsPos)))++"\n" ++
                   numOfErrors,silentAnalysis,errCount)
        where envErrorsPos = errors (rnbStaticAnalysis tree)
              errCount = countError envErrorsPos
              listTree = lines s
              silentAnalysis | length envErrorsPos - errCount > 0 = colorYellow "[STATIC ANALYSIS]: PASS"
                             | otherwise = colorGreen "[STATIC ANALYSIS]: PASS"
              numOfErrors | length envErrorsPos ==0 = colorGreen "[STATIC ANALYSIS]: PASS"
                          |  errCount == 0 =  (warningMsg ("Found "++ (show $ (length envErrorsPos - errCount)) ++ " warnigs."))
                          |  otherwise =  ( errorMsg("Found " ++ (show errCount)) ++ " errors" 
                                ++"\n"++warningMsg ("Found "++ (show $ (length envErrorsPos - errCount)) ++ " warnigs."))


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "   ___  _  _____  _____ ",
      "  / _ \\/ |/ / _ )/ ___/ ",
      " / , _/    / _  / /__   ",
      "/_/|_/_/|_/____/\\___/ompiler   " ,                   
      ""
    , "  --help                Display this help message."
    , "  (files)               Parse content of files verbosely."
    , "  --check (files)       Check the correctness of the RNBStaticAnalysis with the powerful Test-Suite."
    , "  --verbose (files)     Show all prints.  "
    , "  --silent (files)      Remove all prints. "
    , "  --checkTAC (files)    Check if the generated TAC is equal to the expected TAC."
    , "  --nocolor (files)     Print output without colors."
    ]
  exitFailure

emptyInputError :: IO ()
emptyInputError = do
    putStrLn  (errorMsg "You have to provide an input file.")
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    ["--version"] -> putStrLn $ unlines
        [ "   ___  _  _____  _____ ",
          "  / _ \\/ |/ / _ )/ ___/ ",
          " / , _/    / _  / /__   ",
          "/_/|_/_/|_/____/\\___/ompiler   " ,                   
          "","The almighty RNB compiler. V0.1"]

    [] -> emptyInputError
    "--check":fs | checkExtension fs -> mapM_ (compile 6 pProgram) fs
                 | otherwise -> putStrLn (errorMsg "Unrecognized extension.")
    "--checkTAC":fs | checkExtension fs -> mapM_ (compile 5 pProgram) fs
                 | otherwise -> putStrLn (errorMsg "Unrecognized extension.")
    "--verbose":fs | checkExtension fs -> mapM_ (compile 0 pProgram) fs
                 | otherwise -> putStrLn (errorMsg "Unrecognized extension.")
    "--silent":fs | checkExtension fs -> mapM_ (compile 4 pProgram) fs
                 | otherwise -> putStrLn (errorMsg "Unrecognized extension.")
    "--nocolor":fs | checkExtension fs -> mapM_ (compile 7 pProgram) fs
                 | otherwise -> putStrLn (errorMsg "Unrecognized extension.")
    fs | checkExtension fs -> mapM_ (compile 2 pProgram) fs
       | otherwise -> putStrLn (errorMsg "Unrecognized extension.")

checkExtension:: [String] -> Bool
checkExtension [] = True
checkExtension (f:fs) |  ".r" == (drop (length f - 2 )f) = checkExtension fs
                      | otherwise = False


------------------------------------------------------
-- TESTING UNIT
------------------------------------------------------

testing ::Program->String -> IO(String)
testing tree s =  do
 let file = lines s
 let errEx = removeCode file
 let envErrorPos = errors (rnbStaticAnalysis tree)
 let envFilter = removeColor envErrorPos
 let output = test errEx envFilter
 return (output)

removeColor :: [(String,Pos)] -> [(String,Pos)]
removeColor [] = []
removeColor ((s,p):xs) =(fin,p) : removeColor xs
  where fin1 = drop 28 s
        fin2 = takeWhile (/='\ESC') fin1
        fin3 = dropWhile (/='\ESC') fin1
        fin4 = drop 7 fin3
        fin5 = fin2 ++ fin4
        fin = filter (/= '\n') fin5
          
removeCode :: [String] -> [String] 
removeCode [] = []
removeCode (x:xs)
  | isInfixOf "==" x = (drop 2 x) : removeCode xs
  | otherwise = removeCode xs 

test :: [String] -> [(String, Pos)] -> String
test [] [] = colorGreen "[CHECK]:   PASS"
test [] ((s,_):_) =colorRed "[CHECK]:   FAIL " ++  s
test (x:_) [] = colorRed "[CHECK]:   FAIL EXPECTED " ++ x 
test (x:xs) ((s,_):ys) 
  | x == s = test xs ys
  | otherwise = colorRed "[CHECK]:   FAIL " ++ s

removeAnsiList :: [String] -> [String] 
removeAnsiList [] = []
removeAnsiList (x:xs) = (removeAnsi x) : removeAnsiList xs

removeAnsi :: String -> String 
removeAnsi x    
  | (elem '\ESC' x) = removeAnsi y
  | otherwise = x 
       where x1 = takeWhile (/= '\ESC') x
             x2 = dropWhile (/= '\ESC') x 
             x3 = dropWhile (/= 'm') x2
             x4 = drop 1 x3 
             y = x1 ++ x4 

testingTAC :: [TAC] -> String -> String
testingTAC tac prog = testTAC listTAC progTAC
 where stringTAC = prettyTAC tac
       listTAC1 = lines stringTAC
       listTAC = removeAnsiList listTAC1
       progTAC1 = dropWhile (/= '{') prog 
       progTAC2 = drop 2 progTAC1 
       progTAC3 = take (length progTAC2 -2) progTAC2
       progTAC = lines progTAC3 

testTAC :: [String] -> [String] -> String
testTAC [] [] = colorGreen "[CHECK]:   PASS"
testTAC [] (s:_) = colorRed "[CHECK]:   FAIL EXPECTED" ++  s
testTAC (x:_) [] = colorRed "[CHECK]:   FAIL MISSING" ++ x 
testTAC (x:xs) (s:ys) 
  | x == s = testTAC xs ys
  | otherwise = colorRed "[CHECK]:   FAIL \nEXPECTED :" ++  s
      ++ colorRed"\nFOUND  :"++ x 
