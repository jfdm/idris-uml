module UML.ClassDiagram.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.ClassDiagram.Model

%access private

ident : Parser String
ident = map pack (some $ satisfy isAlphaNum)

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

eol : Parser ()
eol = char '\n'

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
-- relation : Parser Relation
description : Parser String
description = do
    colon
    x <- literallyBetween '"'
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

relation : Parser Relation
relation = do
    f <- ident <$ space
    relTy <- relationType
    t <- ident <$ space
    desc <- opt description
    pure $ MkRelation relTy f t desc

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
{-
element : Parser (Either Attribute Method)
element = do a <- attribute
             eol
             pure $ Left a
      <|> do m <- method
             eol
             pure $ Right m
-}
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
parseCD : Parser ClassDiagram
parseCD = do
    cs <- some (clazz <$ space)
    rs <- some (relation <$ space)
    pure $ MkClassDiagram cs rs
  <?> "Class Diagram"
