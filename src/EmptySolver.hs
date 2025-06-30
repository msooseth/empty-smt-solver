import Data.List (isPrefixOf)

main :: IO ()
main = loop
  where
    loop = do
      line <- getLine
      case line of
          "(check-sat)" -> putStrLn "unknown"
          _ | "(set-option" `isPrefixOf` line -> putStrLn "success"
          _ | "(set-info" `isPrefixOf` line -> putStrLn "success"
          _ | "(reset)" `isPrefixOf` line -> putStrLn "success"
            | otherwise -> return ()
      loop

