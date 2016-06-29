import Control.Monad
import Language.Haskell.Interpreter

main :: IO ()
main = do r <- runInterpreter $ compareFunctions "ExampleTemplate" "ExampleCode"
          case r of
            Left err -> printInterpreterError err
            Right () -> putStrLn "Done"
          c <- runInterpreter $ compareFunctions "ExampleCode" "ExampleTemplate"
          case c of
            Left err -> printInterpreterError err
            Right () -> putStrLn "Done"

asf asdf 
subset:: Eq a => [a] -> [a] -> Bool
subset [] y = True
subset (x:xs) y = (subset xs y) && (foldr (||) False $ map (\v -> v == x) y )


compareFunctions:: String -> String -> Interpreter ()
compareFunctions template code = do
    loadModules[template]
    expected <- getModuleExports template
    loadModules[code]
    found <- getModuleExports code
    say $ show $ subset expected found
    

runTestHint :: String -> String -> Interpreter ()
runTestHint a b =
    do
      loadModules [a]
      loadModules [b]
      say "Done!"

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)


checkModule :: String -> IO ()
checkModule name = do 
    r <- runInterpreter $ loadModules [name]
    case r of 
        Left err -> putStrLn "Error"
        Right () -> putStrLn "No Error"

testHint :: Interpreter ()
testHint =
    do
        loadModules ["SomeModule.hs"]
        say "Type of Expression:"
