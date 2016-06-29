import Control.Monad
import Language.Haskell.Interpreter

main :: IO ()
main = do c <- runInterpreter $ compareFunctions "ExampleCode" "ExampleTemplate"
          case c of
            Left err -> putStrLn $ "Error: " ++ err
            Right True -> putStrLn "Done Right"
            Right False -> putStrLn "Done Wrong"

subset:: Eq a => [a] -> [a] -> Bool
subset [] y = True
subset (x:xs) y = (subset xs y) && (foldr (||) False $ map (\v -> v == x) y )




compareFunctions:: String -> String -> Interpreter Bool
compareFunctions template code = do
    loadModules[template]
    expected <- getModuleExports template
    loadModules[code]
    found <- getModuleExports code
    return $ subset expected found
    

say :: String -> Interpreter ()
say = liftIO . putStrLn

checkModule :: String -> IO Bool
checkModule name = do 
    r <- runInterpreter $ loadModules [name]
    case r of 
        Left err -> return False
        Right () -> return True


