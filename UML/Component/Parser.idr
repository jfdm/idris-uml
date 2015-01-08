-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : UML.Component.Parser
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Component.Parser

import Lightyear
import Lightyear.Strings

import UML.Types

import UML.Code.Model
import UML.Code.Parser

import UML.Component.Model
import UML.Utils.Parsing

-- --------------------------------------------------------------- [ Interface ]
actual : Parser $ ComponentModel INTERFACE
actual = do
    token "interface"
    id <- ident <$ space
    fs <- braces (some (funcs <$ space) <$ space)
    pure $ Actual id fs

provides : Parser $ ComponentModel INTERFACE
provides = do
    token "provides"
    id <- ident <$ space
    orig <- (token "from" $> ident)
    space
    pure $ Provided id orig
  <?> "Provides"

requires : Parser $ ComponentModel INTERFACE
requires = do
    token "requires"
    id <- ident <$ space
    token "from"
    orig <- ident <$ space
    pure $ Required id orig
   <?> "Requires"

interface : Parser $ ComponentModel INTERFACE
interface = provides <|> requires <|> actual <?> "Interface"

-- --------------------------------------------------------------- [ Component ]

component : Parser $ ComponentModel COMPONENT
component = do
    token "component"
    name <- ident <$ space
    (is, cs) <- braces (cbody <$ space)
    pure $ MkComponent name is cs
   <?> "Component"
  where
    cbody : Parser (List $ ComponentModel INTERFACE, (List $ ComponentModel COMPONENT))
    cbody = do
      is <- some (interface <$ space)
      cs <- opt $ some (component <$ space)
      pure (is, fromMaybe Nil cs)

-- ----------------------------------------------------------------- [ Diagram ]

public
componentModel : Parser UML
componentModel = do
    ds <- some (dtype <$ space)
    space
    cs <- some (component <$ space)
    pure $ Component $ MkComponentModel ds cs
  <?> "Component Model"

-- --------------------------------------------------------------------- [ EOF ]
