-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : UML.Sequence.Parser
-- Description : Parser for textual UML sequence diagrams.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Sequence.Parser

import Lightyear
import Lightyear.Strings

import UML.Types

import UML.Code.Model
import UML.Code.Parser

import UML.Sequence.Model
import UML.Utils.Parsing

%access private

||| Parse a single step.
step : Parser $ SequenceModel SEND
step = do
    from <- ident <* space
    token "->"
    to <- ident <* space
    colon
    ms <- parens $ commaSep1 attr
    pure $ Send from to ms
  <?> "Step"

||| Parse a sequence diagram, a series of steps.
public
sequenceModel : Parser UML
sequenceModel = do
    ds <- some (dtype <* space)
    space
    ss <- some (step <* space)
    pure $ Sequence $ MkSeqModel ds (mkStep ss)
  <?> "Sequence Model"
-- --------------------------------------------------------------------- [ EOF ]
