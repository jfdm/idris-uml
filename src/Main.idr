module Main

import UML

namespace Flag
  data UML = Clazz | Sequence | Deployment | Component

record Opts : Type where
    MkOpts : (fname : String )
           -> (modelTy : Flag.UML)
           -> Opts


processArgs : List String -> {[EXCEPTION String]} Eff Opts
processArgs [x] = raise "Not enough arguments"
processArgs (x::xs) with (xs)
    | (y::z::ys) = case y of
      "--class"      => pure $ MkOpts z Flag.Clazz
      "--sequence"   => pure $ MkOpts z Flag.Sequence
      "--deployment" => pure $ MkOpts z Flag.Deployment
      "--component"  => pure $ MkOpts z Flag.Component
      otherwise      => raise "Unrecognised flag"
    | Nil = raise "No arguments"

doParse : Parser a -> String -> {[EXCEPTION String]} Eff a
doParse p src  = case parse p src of
    Left err    => raise err
    Right model => pure model

umlMain : {UMLEffs} Eff ()
umlMain = do
    args <- getArgs
    opts <- (processArgs args)
    src <- readFile (fname opts)
    case modelTy opts of
      Flag.Clazz      => putStrLn $ show !(doParse classdiagram src)
      Flag.Sequence   => putStrLn $ show !(doParse sequencediagram src)
      Flag.Deployment => putStrLn $ show !(doParse deploymentdiagram src)
      Flag.Component  => putStrLn $ show !(doParse componentdiagram src)


main : IO ()
main = run umlMain
