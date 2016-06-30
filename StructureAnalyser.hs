module StructureAnalyser where

import Control.Monad
import Language.Haskell.Interpreter

filterModuleElems :: [ModuleElem] -> String -> Interpreter [ModuleElem]
filterModuleElems [] _ = return []
filterModuleElems (x:xs) elemType = do
    n <- typeOf $ name x 
    ys <- filterModuleElems xs elemType
    if(n == elemType)
        then do return (x:ys)
    else do return ys

isModuleCompilable :: String -> Interpreter Bool
isModuleCompilable name = do 
    r <- runInterpreter $ loadModules [name]
    case r of 
        Left err -> return False
        Right () -> return True

compareModuleFunctions :: String -> String -> Interpreter Bool
compareModuleFunctions template code = do
    expected <- getExportList template
    found <- getExportList code
    return $ subset expected found


getExportList :: String -> Interpreter [ModuleElem]
getExportList m = do
    loadModules[m]
    getModuleExports m


subset :: Eq a => [a] -> [a] -> Bool
subset [] y = True
subset (x:xs) y = (subset xs y) && (foldr (||) False $ map (\v -> v == x) y )    