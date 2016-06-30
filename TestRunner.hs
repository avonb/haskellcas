module TestRunner where

import Control.Monad
import Language.Haskell.Interpreter
import Test.HUnit
import StructureAnalyser
import ExampleTest
import ExampleCode
import System.IO ( stderr )

getTestList :: String -> Interpreter [ModuleElem]
getTestList m = do
    l <- getExportList m
    setTopLevelModules [m]
    filterModuleElems l "Test"

main :: IO()
main = do r <- runInterpreter $ getTestList "ExampleTest"
          case r of 
            Left err -> putStrLn $ show err
            Right _ -> putStrLn $ show r
          gradeSubmission
          --c <- runTestTT test1
          putStrLn "Done"


gradeSubmission:: IO Counts
gradeSubmission = do
    (c, _) <- runTest test1
    putStrLn $ show c
    return c
    --putStrLn $ show c
    --return c

runTest:: Test -> IO (Counts, Int)
runTest t = runTestText (PutText f 0) t 
  where f :: String -> Bool -> Int -> IO Int
        f _ _ _ = return 0