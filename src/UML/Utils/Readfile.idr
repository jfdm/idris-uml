-- ------------------------------------------------------------ [ Readfile.idr ]
-- Module      : UML.Utils.Readfile
-- Description : Read file into memory
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Utils.Readfile

import UML.Effs

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
readFile : (fname : String) -> {UMLEffs} Eff String
readFile fname = do
    case !(open fname Read) of
      True => do
        src <- consumeFile
        close
        pure src
      False => raise "Unable to read file"
-- --------------------------------------------------------------------- [ EOF ]
