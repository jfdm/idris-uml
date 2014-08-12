module Main

import Effects
import Effect.StdIO
import Effect.System
import Effect.Exception

import UML

dia : String
dia = "Class Bob {\n + ad : String\n}\nClass Alice\nBob --> Alice\n"

dia2 : String
dia2 = "A -> B : Syn\n B -> A : Ack\n A -> B : Syn, Ack\n"

umlMain : {[SYSTEM, EXCEPTION String, STDIO]} Eff ()
umlMain = do
    args <- getArgs
    case parse parseSD dia2 of
      Left err => raise "Oops"
      Right d  => putStrLn $ show d

main : IO ()
main = run umlMain
