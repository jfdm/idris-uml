module UML.Component.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Component.Model

ident : Parser String
ident = map pack (some $ satisfy isAlphaNum)

-- --------------------------------------------------------------- [ Interface ]

provides : Parser Interface
provides = do
    token "provides"
    id <- ident <$ space
    orig <- opt (token "from" $> ident)
    space
    pure $ Provides id orig
  <?> "Provides"

requires : Parser Interface
requires = do
    token "requires"
    id <- ident <$ space
    token "from"
    orig <- ident <$ space
    pure $ Requires id orig
   <?> "Requires"

-- --------------------------------------------------------------- [ Component ]

component : Parser Component
component = do
    token "component"
    name <- ident <$ space
    (ps, rs, cs) <- braces (cbody <$ space)
    pure $ MkComponent name ps rs cs
   <?> "Component"
  where
    cbody : Parser (Interfaces, Maybe Interfaces, Maybe (List Component))
    cbody = do
      ps <- some (provides <$ space)
      rs <- opt $ some (requires <$ space)
      cs <- opt $ some (component <$ space)
      pure (ps, rs, cs)

-- ----------------------------------------------------------------- [ Diagram ]

componentdiagram : Parser ComponentDiagram
componentdiagram = do
    cs <- space $> some (component <$ space)
    pure cs
  <?> "Component Diagram"
-- --------------------------------------------------------------------- [ EOF ]
