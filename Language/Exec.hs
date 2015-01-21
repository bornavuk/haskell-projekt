module Exec
(runHashProgram, Command, ScriptState(ScriptState), wd, vartable)
where

--import Control.Exception
--import Control.Monad
import qualified Data.Map as M
import Expressions

-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output :: String
                               , wd :: FilePath
                               , vartable :: VarTable
                               } deriving Show

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram ctable (Left fp) tlexprs = do
    runHashProgram ctable (Right (ScriptState "" fp M.empty)) tlexprs
runHashProgram _ (Right ss) [] = return ss
runHashProgram ctable (Right ss) (tlexpr:tlexprs) = do
    ss' <- runTopLevel ctable ss tlexpr
    runHashProgram ctable (Right ss') tlexprs

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel ctable ss (TLCmd (Cmd name args)) = do
    let cname = decodeExpr (vartable ss) (name)
    let cargs = map (decodeExpr (vartable ss)) (args)
    case (M.lookup cname ctable) of
        Nothing -> do
            putStrLn "nepoznata komanda"
            return ss
        Just x -> do
            ss' <- x cargs ss
            putStrLn $ output ss'
            return ss'
runTopLevel _ (ScriptState _ wd vartable) (TLCmd (Assign (Str var) (Str val))) = do
    return $ ScriptState "" wd $ M.insert var val vartable
runTopLevel _ (ScriptState _ wd vartable) (TLCmd (Assign (Str var) val)) = do
    return $ ScriptState "" wd $ M.insert var (decodeExpr vartable val) vartable
runTopLevel ctable ss@(ScriptState out wd vartbl) (TLCnd (If cond cthen)) = do
    if decodePred vartbl cond then do
        ss' <- runHashProgram ctable (Right ss) (map TLCmd cthen)
        return ss'
    else do
        return ss
runTopLevel ctable ss@(ScriptState out wd vartbl) (TLCnd (IfElse cond cthen celse)) = do
    if decodePred vartbl cond then do
        ss' <- runHashProgram ctable (Right ss) (map TLCmd cthen)
        return ss'
    else do
        ss' <- runHashProgram ctable (Right ss) (map TLCmd celse)
        return ss'

decodeExpr :: VarTable -> Expr -> String
decodeExpr _ (Str cname) = cname
decodeExpr vartbl (Var cname) = case (M.lookup cname vartbl) of
                                    Just x -> x
                                    Nothing -> ""

decodePred :: VarTable -> Pred -> Bool
decodePred vartbl (Pred comp) = decodeComp vartbl comp
decodePred vartbl (Not pred) = not $ decodePred vartbl pred
decodePred vartbl (And pred1 pred2) = decodePred vartbl pred1 && (decodePred vartbl pred2)
decodePred vartbl (Or pred1 pred2) = decodePred vartbl pred1 || (decodePred vartbl pred2)
decodePred vartbl (Parens pred) = decodePred vartbl pred

decodeComp :: VarTable -> Comp -> Bool
decodeComp vartbl (CEQ ex1 ex2) = decodeExpr vartbl ex1 == (decodeExpr vartbl ex2)
decodeComp vartbl (CNE ex1 ex2) = decodeExpr vartbl ex1 /= (decodeExpr vartbl ex2)
decodeComp vartbl (CGE ex1 ex2) = decodeExpr vartbl ex1 >= (decodeExpr vartbl ex2)
decodeComp vartbl (CGT ex1 ex2) = decodeExpr vartbl ex1 > (decodeExpr vartbl ex2)
decodeComp vartbl (CLE ex1 ex2) = decodeExpr vartbl ex1 <= (decodeExpr vartbl ex2)
decodeComp vartbl (CLT ex1 ex2) = decodeExpr vartbl ex1 < (decodeExpr vartbl ex2)
decodeComp vartbl (CLI ex) = not . null $ decodeExpr vartbl ex


-- The rest of the module should consist of similar functions, calling each
-- other so that each expression is parsed by a lower-level function and the
-- result can be used in a higher-level function. The Command table and state
-- are passed around as necessary to evaluate commands, assignments and
-- variable substitution. A better way to pass around variables would be to
-- use the State monad or even the StateT monad transformer to wrap IO into it.

