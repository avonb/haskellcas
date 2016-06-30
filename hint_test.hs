import Control.Monad
import Language.Haskell.Interpreter
import Test.HUnit

mai2 :: IO String
mai2 = do c <- runInterpreter $ compareModuleFunctions "ExampleCode" "ExampleTemplate"
          case c of
            Left err -> return "Error: "
            Right True -> return "Done Right"
            Right False -> return "Done Wrong"


main :: IO()
main = do r <- runInterpreter $ getTestList "ExampleTest"
          case r of 
            Left err -> putStrLn $ show err
            Right _ -> putStrLn $ show r

getTestList :: String -> Interpreter [ModuleElem]
getTestList m = do
    l <- getExportList m
    setTopLevelModules [m]
    filterModule l "Test"

filterModule :: [ModuleElem] -> String -> Interpreter [ModuleElem]
filterModule [] _ = return []
filterModule (x:xs) elemType = do
    n <- typeOf $ name x 
    ys <- filterModule xs elemType
    case n of
       elemType -> return (x:ys)
       _ ->  return ys

    

getExportList :: String -> Interpreter [ModuleElem]
getExportList m = do
    loadModules[m]
    getModuleExports m

subset :: Eq a => [a] -> [a] -> Bool
subset [] y = True
subset (x:xs) y = (subset xs y) && (foldr (||) False $ map (\v -> v == x) y )


compareModuleFunctions :: String -> String -> Interpreter Bool
compareModuleFunctions template code = do
    expected <- getExportList template
    found <- getExportList code
    return $ subset expected found
    

say :: String -> Interpreter ()
say = liftIO . putStrLn

isModuleCompilable :: String -> Interpreter Bool
isModuleCompilable name = do 
    r <- runInterpreter $ loadModules [name]
    case r of 
        Left err -> return False
        Right () -> return True


