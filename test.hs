main:: IO()
main = putStrLn $ show $ subset [1,2,3] [1,3,4,5,2]

subset:: Eq a => [a] -> [a] -> Bool
subset [] y = True
subset (x:xs) y = (subset xs y) && (foldr (||) False $ map (\v -> v == x) y )