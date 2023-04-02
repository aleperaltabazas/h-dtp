module REPL.Console where

import           System.IO                      ( hFlush
                                                , stdout
                                                )

newline :: IO ()
newline = putStrLn ""

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

promptConfirmation :: String -> IO Bool
promptConfirmation text = prompt (text ++ " (y/n): ") >>= confirm

confirm :: String -> IO Bool
confirm "y" = return True
confirm "n" = return False
confirm _   = prompt "Please, type 'y' or 'n': " >>= confirm
