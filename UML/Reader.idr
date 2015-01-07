-- -------------------------------------------------------------- [ Reader.idr ]
-- Module      : UML.Reader
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
--
-- Reading Utilities
-- --------------------------------------------------------------------- [ EOH ]
module UML.Reader

import public Effects
import public Effect.Exception
import public Effect.File

import Lightyear
import Lightyear.Strings

import UML.Class
import UML.Sequence
import UML.Deployment
import UML.Component
import UML.Code

import UML.Types

import public UML.Utils.Reading

%access public
-- ------------------------------------------------------------------------ [  ]
private
doParse : Parser UML -> String -> {[EXCEPTION String]} Eff UML
doParse p src = case parse p src of
    Left err    => raise err
    Right model => pure model

||| Read a UML file from string.
readUMLString : UMLTy
              -> String
              -> {[EXCEPTION String]} Eff UML
readUMLString CLAZZ      = doParse classModel
readUMLString SEQUENCE   = doParse sequenceModel
readUMLString DEPLOYMENT = doParse deploymentModel
readUMLString COMPONENT  = doParse componentModel


||| Read a UML model from a file
readUMLFile : UMLTy
            -> String
            -> {[EXCEPTION String, FILE_IO ()]} Eff UML
readUMLFile ty fname = do
    src <- readFile fname
    model <- readUMLString ty src
    pure model
-- --------------------------------------------------------------------- [ EOF ]
