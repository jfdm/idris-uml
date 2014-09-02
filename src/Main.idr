module Main

import UML

processArgs : List String -> Maybe String
processArgs [x]       = Nothing
processArgs (x::y::z) = Just y

umlMain : {UMLEffs} Eff ()
umlMain = do
    args <- getArgs
    case processArgs args of
      Nothing => raise "Error parsing args"
      Just fname => do
        src <- readFile fname
        case parse parseDD src of
          Left err => putStrLn err
          Right d  => putStrLn $ show d

main : IO ()
main = run umlMain
