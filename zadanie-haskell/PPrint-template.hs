module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = undefined

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS undefined
pprH = intercalateS undefined

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep list = undefined

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith = undefined

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
