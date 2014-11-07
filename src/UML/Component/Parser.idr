-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : UML.Component.Parser
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Component.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Code.Model
import UML.Code.Parser

import UML.Component.Model
import UML.Utils.Parsing

-- --------------------------------------------------------------- [ Interface ]
actual : Parser Interface
actual = do
    token "interface"
    id <- ident <$ space
    fs <- braces (some (funcs <$ space) <$ space)
    pure $ Actual id fs

provides : Parser Interface
provides = do
    token "provides"
    id <- ident <$ space
    orig <- (token "from" $> ident)
    space
    pure $ Provided id orig
  <?> "Provides"

requires : Parser Interface
requires = do
    token "requires"
    id <- ident <$ space
    token "from"
    orig <- ident <$ space
    pure $ Required id orig
   <?> "Requires"

interface : Parser Interface
interface = provides <|> requires <|> actual <?> "Interface"

-- --------------------------------------------------------------- [ Component ]

component : Parser Component
component = do
    token "component"
    name <- ident <$ space
    (is, cs) <- braces (cbody <$ space)
    pure $ MkComponent name is cs
   <?> "Component"
  where
    cbody : Parser (Interfaces, Maybe (List Component))
    cbody = do
      is <- some (interface <$ space)
      cs <- opt $ some (component <$ space)
      pure (is, cs)

-- ----------------------------------------------------------------- [ Diagram ]

public
componentdiagram : Parser ComponentDiagram
componentdiagram = do
    ds <- some (dtype <$ space)
    space
    cs <- some (component <$ space)
    pure $ MkComponentDiagram ds cs
  <?> "Component Diagram"

-- --------------------------------------------------------------------- [ EOF ]
