module UML.DeployDiagram.Parser

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import UML.DeployDiagram.Model


ident : Parser String
ident = map pack (some $ satisfy isAlphaNum)

literallyBetween : Char -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

eol : Parser ()
eol = char '\n'

-- -------------------------------------------------------------------- [ Misc ]
kvpair : Parser Attribute
kvpair = do
    key <- ident <$ space
    colon
    value <- literallyBetween '\"'
    pure (key, value)
  <?> "KV Pair"

kvBody : Parser Attributes
kvBody = braces $ commaSep1 kvpair
  <?> "KV Body"
-- --------------------------------------------------------------- [ Relations ]

comms : Parser Relation
comms = do
    xID <- ident <$ space
    token "commsWith"
    yID <- ident <$ space
    token "via"
    proto <- ident <$ space
    pure $ Comms xID yID proto
  <?> "Communicates with"

runs : Parser Relation
runs = do
    xID <- ident <$ space
    token "runs"
    yID <- ident <$ space
    pure $ Runs xID yID
  <?> "Run Artifact"

hosts : Parser Relation
hosts = do
    xID <- ident <$ space
    token "hosts"
    yID <- ident <$ space
    pure $ Hosts xID yID
  <?> "Hosts Execution Environment"

specs : Parser Relation
specs = do
    xID <- ident <$ space
    token "specs"
    yID <- ident <$ space
    pure $ AssignSpec xID yID
  <?> "Assign Specification"

relation : Parser Relation
relation = specs <|> hosts <|> runs <|> comms <?> "Relation"

-- ----------------------------------------------------------- [ Specification ]

spec : Parser Specification
spec = do
    token "specification"
    id <- ident <$ space
    as <- kvBody
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
    pure $ MkArtifact ty id

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
    as <- opt kvBody
    pure $ MkEnv ty id as
  <?> "Execution Environment"

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
    as <- opt kvBody
    pure $ MkDevice (fromMaybe GenericDev ty) id as


public
parseDD : Parser DeployDiagram
parseDD = do
    ds <- some (device <$ space)
    es <- some (exenv <$ space)
    as <- some (artifact <$ space)
    ss <- opt $ some (spec <$ space)
    rs <- some (relation <$ space)
    pure $ MkDeployDiagram ds es as ss rs
  <?> "Deployment Diagram"
-- --------------------------------------------------------------------- [ EOF ]
