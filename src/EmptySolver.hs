import Control.Monad (when)

main :: IO ()
main = do
    line <- getLine
    when (line == "(check-sat)") $ putStrLn "unknown"
    main
