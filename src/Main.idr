module Main

import Effects
import Effect.StdIO
import Effect.System

import UML

dia : String
dia = "Class Bob {\n + ad : String\n}\nClass Alice\nBob --> Alice\n"


umlMain : {[SYSTEM, STDIO]} Eff ()
umlMain = do
    args <- getArgs
    case parse parseCD dia of
      Left err => putStrLn "OOps"
      Right d  => putStrLn $ show d

main : IO ()
main = run umlMain
