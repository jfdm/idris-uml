module UML.Component.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Component.Model
import UML.Utils.Parsing

%access private

-- -------------------------------------------------------------------- [ Data ]
attr : Parser (String, String)
attr = do
    n <- ident <$ space
    colon
    ty <- ident <$ space
    pure $ MkPair n ty
  <?> "Parse Attribute"

simpleDtype : Parser DType
simpleDtype = do
    token "data"
    id <- ident <$ space
    pure $ MkSType id
   <?> "Simple Data Type"

complexDtype : Parser DType
complexDtype = do
    token "data"
    id <- ident <$ space
    body <- braces (commaSep1 (attr <$ space) <$ space)
    pure $ MkCType id body
   <?> "Complex Data Type"

dtype : Parser DType
dtype = complexDtype <|> simpleDtype <?> "Data Types"

-- --------------------------------------------------------------- [ Functions ]

paramfunc : Parser Function
paramfunc = do
    id <- ident <$ space
    colon
    ps <- some (parens attr <$ token "->")
    rty <- ident <$ space
    pure $ MkPFunc id ps rty
  <?> "Func with parameters"

noparamfunc : Parser Function
noparamfunc = do
    id <- ident <$ space
    colon
    rty <- ident
    pure $ MkFunc id rty

funcs : Parser Function
funcs = paramfunc <|> noparamfunc

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
