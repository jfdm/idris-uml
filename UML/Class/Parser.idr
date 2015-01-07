-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : UML.Class.Parser
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Class.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Class.Model

import UML.Utils.Parsing

%access private
-- -------------------------------------------------------------------- [ Misc ]
modifier : Parser Modifier
modifier = do token "static"
              pure Static
       <|> do token "abstract"
              pure Abstract
       <?> "Modifier"

visibility : Parser Visibility
visibility = do token "-"
                pure Private
         <|> do token "+"
                pure Public
         <|> do token "~"
                pure Package
         <|> do token "#"
                pure Protected
         <?> "Visibility"

-- ---------------------------------------------------------------- [ Relation ]
description : Parser String
description = do
    colon
    x <- literallyBetween '\"'
    pure x

relationType : Parser RelationTy
relationType = do token "<|--"
                  pure Specialisation
           <|> do token "o--"
                  pure Composition
           <|> do token "*--"
                  pure Aggregation
           <|> do token "<--"
                  pure Realisation
           <|> do token "-->"
                  pure Association
           <?> "Relation Type"

relation : Parser ClassRelation
relation = do
    f <- ident <$ space
    relTy <- relationType
    t <- ident <$ space
    desc <- opt description
    pure $ MkClassRelation relTy f t desc

-- ------------------------------------------------------------------ [ Params ]

param : Parser Param
param = do
    id <- ident <$ space
    colon
    ty <- ident <$ space
    pure $ MkParam id ty
  <?> "Param"

params : Parser Params
params = sepBy1 param comma <?> "Params"

-- ----------------------------------------------------------------- [ Methods ]

method : Parser Method
method = do
    mod <- opt $ braces modifier
    vis <- visibility
    id  <- ident
    ps  <- parens $ opt params
    colon
    ty <- ident
    pure $ MkMethod id ty mod vis ps
  <?> "Method"

-- -------------------------------------------------------------- [ Attributes ]

attribute : Parser Attribute
attribute = do
    mod <- opt $ braces modifier
    vis <- visibility
    id  <- ident <$ space
    colon
    ty  <- ident
    pure $ MkAttribute id ty mod vis
  <?> "Attribute"

-- ------------------------------------------------------------------- [ Class ]

classty : Parser ClassTy
classty = do token "Abstract"
             token "Class"
             return ClassAbstract
      <|> do token "Class"
             return ClassStandard
      <|> do token "Interface"
             return ClassInterface
      <?> "Class Type"

classDecl : Parser (ClassTy, String)
classDecl = do
    t <- classty
    i <- ident
    space
    pure (t,i)
  <?> "Class Delcaration"


emptyClass : Parser Class
emptyClass = do
    (ty,id) <- classDecl
    let c = MkClass id ty Nothing Nothing
    pure c
  <?> "Empty Class"


element : Parser (Maybe Attribute, Maybe Method)
element = do a <- attribute
             eol
             pure (Just a, Nothing)
      <|> do m <- method
             eol
             pure (Nothing, Just m)
      <?> "Element"

classBody : Parser (Attributes, Methods)
classBody = do
    es <- some element
    let (as', ms') = unzip es
    pure (catMaybes as', catMaybes ms')

bodyClass : Parser Class
bodyClass = do
    (ty, id) <- classDecl
    (as, ms) <- braces classBody
    let c = MkClass id ty (Just as) (Just ms)
    pure c

clazz : Parser Class
clazz = bodyClass <|> emptyClass <?> "Class"

-- ----------------------------------------------------------- [ Class Diagram ]

public
classModel : Parser ClassModel
classModel = do
    cs <- some (clazz <$ space)
    rs <- some (relation <$ space)
    pure $ MkClassModel cs rs
  <?> "Class Diagram"
-- --------------------------------------------------------------------- [ EOF ]
