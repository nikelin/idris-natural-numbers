module Main

import Functions

main : IO ()

main = do
    _ <- putStrLn (Strings.(++) "Natural numbers multiplication result (5 * 6): " (show (mult nat_five nat_six)))
    putStrLn (Strings.(++) "Natural numbers multiplication result (5 + 6): " (show (plus nat_five nat_six)))

