-- ------------------------------------------------------------ [ Readfile.idr ]
-- Module      : UML.Utils.Readfile
-- Description : Read file into memory
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Utils.Reading

import Effects
import Effect.StdIO
import Effect.System
import Effect.Exception
import Effect.File

%access private

||| Do the consuming.
consumeFile : { [FILE_IO (OpenFile Read)] } Eff String
consumeFile = readAcc ""
  where
    readAcc : String -> { [FILE_IO (OpenFile Read)] } Eff String
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

||| Read file...
public
readFile : (fname : String) -> {[EXCEPTION String, FILE_IO ()]} Eff String
readFile fname = do
    case !(open fname Read) of
      True => do
        src <- consumeFile
        close
        pure src
      False => raise "Unable to read file"
-- --------------------------------------------------------------------- [ EOF ]
