-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : UML.Class.Parser
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Class.Parser

import Lightyear
import Lightyear.Strings

import UML.Class.Model
import UML.Types

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

relation : Parser $ ClassModel RELA
relation = do
    f <- ident <$ space
    relTy <- relationType
    t <- ident <$ space
    desc <- opt description
    pure $ Relation relTy f t desc

-- ------------------------------------------------------------------ [ Params ]

param : Parser $ ClassModel PARAM
param = do
    id <- ident <$ space
    colon
    ty <- ident <$ space
    pure $ Param id ty
  <?> "Param"

params : Parser $ List $ ClassModel PARAM
params = sepBy1 param comma <?> "Params"

-- ----------------------------------------------------------------- [ Methods ]

method : Parser $ ClassModel METHOD
method = do
    mod <- opt $ braces modifier
    vis <- visibility
    id  <- ident
    ps  <- parens $ opt params
    colon
    ty <- ident
    pure $ Method id ty mod vis $ fromMaybe Nil ps
  <?> "Method"

-- -------------------------------------------------------------- [ Attributes ]

attribute : Parser $ ClassModel ATTR
attribute = do
    mod <- opt $ braces modifier
    vis <- visibility
    id  <- ident <$ space
    colon
    ty  <- ident
    pure $ Attribute id ty mod vis
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


emptyClass : Parser $ ClassModel CLASS
emptyClass = do
    (ty,id) <- classDecl
    let c = Clazz id ty Nil Nil
    pure c
  <?> "Empty Class"


element : Parser (Maybe $ ClassModel ATTR, Maybe $ ClassModel METHOD)
element = do a <- attribute
             eol
             pure (Just a, Nothing)
      <|> do m <- method
             eol
             pure (Nothing, Just m)
      <?> "Element"

classBody : Parser (List $ ClassModel ATTR, List $ ClassModel METHOD)
classBody = do
    es <- some element
    let (as', ms') = unzip es
    pure (catMaybes as', catMaybes ms')

bodyClass : Parser $ ClassModel CLASS
bodyClass = do
    (ty, id) <- classDecl
    (as, ms) <- braces classBody
    let c = Clazz id ty as ms
    pure c

clazz : Parser $ ClassModel CLASS
clazz = bodyClass <|> emptyClass <?> "Class"

-- ----------------------------------------------------------- [ Class Diagram ]

public
classModel : Parser UML
classModel = do
    cs <- some (clazz <$ space)
    rs <- some (relation <$ space)
    pure $ Class $ MkClassModel cs rs
  <?> "Class Diagram"
-- --------------------------------------------------------------------- [ EOF ]
