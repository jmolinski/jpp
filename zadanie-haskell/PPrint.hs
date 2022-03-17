module PPrint where

import Data.List

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k, v) = showString k . showString ": " . showString (show v)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showString "\n"
pprH = intercalateS $ showChar ' '

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep list = foldr (.) id $ intersperse sep list

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith fn l = pprV $ map fn l 

runShows :: ShowS -> IO ()
runShows = putStrLn . ($ "")
