main :: IO ()
main = putStrLn "Hello, Haskell!"

sillyExchange :: IO ()
sillyExchange = do
  putStrLn "Hello, user!"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"