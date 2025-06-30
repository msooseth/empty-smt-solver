import Data.List (isPrefixOf)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  loop
  where
    loop = do
      line <- getLine
      case line of
          "(check-sat)" -> putStrLn "unknown"
          _ | "(set-option" `isPrefixOf` line -> putStrLn "success"
          _ | "(set-info" `isPrefixOf` line -> putStrLn "success"
          _ | "(set-logic" `isPrefixOf` line -> putStrLn "success"
          _ | "(reset)" `isPrefixOf` line -> putStrLn "success"
            | otherwise -> return ()
      hFlush stdout
      loop

