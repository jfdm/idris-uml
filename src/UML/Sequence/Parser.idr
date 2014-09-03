module UML.Sequence.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Sequence.Model

%access private

ident : Parser String
ident = map pack (some $ satisfy isAlphaNum)

eol : Parser ()
eol = char '\n'

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

public
sequencediagram : Parser SequenceDiagram
sequencediagram = do
    ss <- space $> some (step <$ space)
    pure ss
  <?> "Sequence Diagram"
