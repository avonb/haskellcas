module ExampleTest where

import ExampleCode 
import Test.HUnit

test1 = TestCase (assertEqual "Bla" 2 (add1 1))
test2 = TestCase (assertEqual "Bla" 3 (add1 2))
test3 = TestCase (assertEqual "Bla" 4 (add1 3))
test4 = TestCase (assertEqual "Bla" 5 (add1 4))
