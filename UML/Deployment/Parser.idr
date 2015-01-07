-- -------------------------------------------------------------- [ Parser.idr ]
-- Module      : UML.Deployment.Parser
-- Description : Parser for textual UML deployment diagrams.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Deployment.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.Deployment.Model
import UML.Utils.Parsing

%access private
-- -------------------------------------------------------------------- [ Misc ]
||| Parse a key-value pair.
|||
||| Key value pairs are of the form: `<key> : "<value>"`.
|||
kvpair : Parser Attribute
kvpair = do
    key <- ident <$ space
    colon
    value <- literallyBetween '\"'
    pure (key, value)
  <?> "KV Pair"

||| Parse pair of braces containing many comma separated KV pairs.
kvBody : Parser Attributes
kvBody = braces $ (commaSep1 (kvpair <$ space) <$ space)
  <?> "KV Body"

||| Parse several properties.
properties : Parser Attributes
properties = do
    token "properties"
    ps <- kvBody
    pure ps
  <?> "Properties"

-- --------------------------------------------------------------- [ Relations ]
||| Parse a relation defined between two entities.
relation : Parser Relation
relation = do
    xID <- ident <$ space
    token "commsWith"
    yID <- ident <$ space
    token "via"
    proto <- ident <$ space
    pure $ MkRelation xID yID proto
  <?> "Relation"

-- ----------------------------------------------------------- [ Specification ]

spec : Parser Specification
spec = do
    token "spec"
    id <- ident <$ space
    as <- opt kvBody
    pure $ MkSpec id as

-- ---------------------------------------------------------------- [ Artifact ]

artifactTy : Parser ArtifactTy
artifactTy = do token "doc"
                pure Document
         <|> do token "src"
                pure Source
         <|> do token "lib"
                pure Library
         <|> do token "exe"
                pure Executable
         <|> do token "script"
                pure Script
         <?> "Artifact Type"

artifact : Parser Artifact
artifact = do
    token "artifact"
    ty <- artifactTy
    id <- ident <$ space
    s <- opt artBody
    pure $ MkArtifact ty id s
   <?> "Artifact"
  where
    artBody : Parser (Specification)
    artBody = do
      token "with" <$ space
      s <- spec
      pure s

-- ----------------------------------------------------------------- [ Exe Env ]

envTy : Parser EnvTy
envTy = do token "os"
           pure OS
    <|> do token "engine"
           pure Engine
    <|> do token "container"
           pure Container
    <|> do token "appserver"
           pure AppServer
    <|> do token "app"
           pure App
    <?> "ExeEnv Type"

exenv : Parser Env
exenv = do
    token "env"
    ty <- envTy <$ space
    id <- ident <$ space
    (as, ps) <- braces (exenvBody <$ space)
    pure $ MkEnv ty id as ps
   <?> "Execution Environment"
  where
    exenvBody : Parser (Artifacts, Maybe Attributes)
    exenvBody = do
      as <- some (artifact <$ space)
      ps <- opt properties <$ space
      pure (as, ps)

-- ----------------------------------------------------------------- [ Devices ]
devTy : Parser DeviceTy
devTy = do token "embedded"
           pure Embedded
    <|> do token "mobile"
           pure Mobile
    <|> do token "workstation"
           pure Workstation
    <|> do token "server"
           pure Server
    <?> "Device Type"

device : Parser Device
device = do
    token "device"
    ty <- opt devTy
    id <- ident <$ space
    (es, ps) <- braces $ deviceBody
    pure $ MkDevice (fromMaybe GenericDev ty) id es ps
   <?> "Device"
  where
    deviceBody : Parser (Envs, Maybe Attributes)
    deviceBody = do
      es <- some (exenv <$ space)
      ps <- opt properties <$ space
      pure (es, ps)

public
deploymentModel : Parser DeploymentModel
deploymentModel = do
    ds <- some (device <$ space)
    rs <- some (relation <$ space)
    pure $ MkDeployment ds rs
  <?> "Deployment Model"
-- --------------------------------------------------------------------- [ EOF ]
