module Hello (hello) where

import Unicode.Char.General (isAlphabetic)

hello :: IO ()
hello = print (isAlphabetic 'λ')
