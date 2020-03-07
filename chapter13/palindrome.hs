import Control.Monad (forever)
import System.Exit   (exitSuccess)
import Data.Char (isLetter, toLower)
import Data.Maybe (mapMaybe)

simplifyChar :: Char -> Maybe Char
simplifyChar c = if isLetter c
                 then Just( toLower c)
		 else Nothing

simplifyLine :: String -> String
simplifyLine line = mapMaybe simplifyChar line 

palindrome :: String -> Bool
palindrome line = (simpleline == reverse simpleline)
  where simpleline = simplifyLine line

palindromeGame :: IO ()
palindromeGame = forever $ do
  line1 <- getLine
  case (palindrome line1) of
    True -> do
      putStrLn "It is a palindrome!"
      exitSuccess
    False -> putStrLn "Nope!"
