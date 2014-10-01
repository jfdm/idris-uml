module UML.Component.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Component.Model
import UML.Utils.Parsing

ident : Parser String
ident = map pack (some $ satisfy isAlphaNum)


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

-- --------------------------------------------------------------- [ Component ]

component : Parser Component
component = do
    token "component"
    name <- ident <$ space
    (as, ps, rs, cs) <- braces (cbody <$ space)
    pure $ MkComponent name ps as rs cs
   <?> "Component"
  where
    cbody : Parser (Maybe Interfaces, Maybe Interfaces, Maybe Interfaces, Maybe (List Component))
    cbody = do
      as <- opt $ some (actual <$ space)
      ps <- opt $ some (provides <$ space)
      rs <- opt $ some (requires <$ space)
      cs <- opt $ some (component <$ space)
      pure (as, ps, rs, cs)

-- ----------------------------------------------------------------- [ Diagram ]

componentdiagram : Parser ComponentDiagram
componentdiagram = do
    ds <- some (dtype <$ space)
    space
    cs <- some (component <$ space)
    pure $ MkComponentDiagram ds cs
  <?> "Component Diagram"
-- --------------------------------------------------------------------- [ EOF ]
