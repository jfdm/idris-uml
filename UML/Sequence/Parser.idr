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

import UML.Sequence.Model
import UML.Utils.Parsing

%access private

||| Parse a single step.
step : Parser MessageStep
step = do
    from <- ident <$ space
    token "->"
    to <- ident <$ space
    colon
    ms <- commaSep1 (ident)
    eol
    pure $ MkStep from to ms
  <?> "Step"

||| Parse a sequence diagram, a series of steps.
public
sequenceModel : Parser UML
sequenceModel = do
    ss <- space $> some (step <$ space)
    pure $ Sequence ss
  <?> "Sequence Model"
-- --------------------------------------------------------------------- [ EOF ]
