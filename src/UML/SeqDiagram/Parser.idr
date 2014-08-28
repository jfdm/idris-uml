module UML.SeqDiagram.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.SeqDiagram.Model

%access public

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
parseSD : Parser SeqDiagram
parseSD = do
    ss <- some (step <$ space)
    pure ss
  <?> "Parse Sequence Diagram"
