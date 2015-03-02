-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : UML.Code.Parser
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Code.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Code.Model
import UML.Utils.Parsing

%access public

-- -------------------------------------------------------------------- [ Data ]
attr : Parser (String, String)
attr = do
    n <- ident <* space
    colon
    ty <- ident <* space
    pure $ MkPair n ty
  <?> "Parse Attribute"

simpleDtype : Parser DType
simpleDtype = do
    token "data"
    id <- ident <* space
    pure $ MkSType id
   <?> "Simple Data Type"

complexDtype : Parser DType
complexDtype = do
    token "data"
    id <- ident <* space
    body <- braces (commaSep1 (attr <* space) <* space)
    pure $ MkCType id body
   <?> "Complex Data Type"

dtype : Parser DType
dtype = complexDtype <|> simpleDtype <?> "Data Types"

-- --------------------------------------------------------------- [ Functions ]

paramfunc : Parser Function
paramfunc = do
    id <- ident <* space
    colon
    ps <- some (parens attr <* token "->")
    rty <- ident <* space
    pure $ MkPFunc id ps rty
  <?> "Func with parameters"

noparamfunc : Parser Function
noparamfunc = do
    id <- ident <* space
    colon
    rty <- ident
    pure $ MkFunc id rty

funcs : Parser Function
funcs = paramfunc <|> noparamfunc

-- --------------------------------------------------------------------- [ EOF ]
