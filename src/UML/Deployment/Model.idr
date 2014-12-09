-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Deployment.Model
-- Description : Model for textual UML deployment diagrams.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Deployment.Model

%access public

||| The different types of device type supported.
data DeviceTy = GenericDev | Server | Workstation | Mobile | Embedded

-- ReThink
||| The different types of execution environments supported.
data EnvTy = OS | Engine | Container | AppServer | App

||| The different types of artifacts supported.
data ArtifactTy = Document | Source | Library | Executable | Script

||| Key value pair.
Attribute : Type
Attribute = (String, String)

||| Key value pairs.
Attributes : Type
Attributes = List Attribute

-- ---------------------------------------------------------- [ Specifications ]
||| Description of a named deployment specification for artifacts.
data Specification : Type where
  ||| Construct a new deployment specification.
  |||
  ||| @ident The name of the deployment specification,
  ||| @as The specification as KV pairs.
  MkSpec : (ident : String)
         -> (as : Maybe Attributes)
         -> Specification

-- --------------------------------------------------------------- [ Artifacts ]

||| Resources that are to be deployed in execution environments on devices.
data Artifact : Type where
  ||| Construct a new artifact.
  |||
  ||| @ty The type of artifact.
  ||| @ident The identify of the artifact.
  ||| @spec A possible specification schema that describes deployment.
  MkArtifact : (ty : ArtifactTy)
             -> (ident : String)
             -> (spec : Maybe Specification)
             -> Artifact

Artifacts : Type
Artifacts = List Artifact

-- -------------------------------------------------------------------- [ Envs ]

||| A execution environment in which artifacts are run or hosted.
data Env : Type where
  ||| Construct an execution environment.
  |||
  ||| @ty The type of environment.
  ||| @ident The identity of the environment.
  ||| @as The artifacts being executed/hosted within the environment.
  ||| @ps A possible set of properties for the environment.
  MkEnv : (ty : EnvTy)
        -> (ident : String)
        -> (as : Artifacts)
        -> (ps : Maybe Attributes)
        -> Env

Envs : Type
Envs = List Env

-- ----------------------------------------------------------------- [ Devices ]

||| A physical device in the deployment diagram.
data Device : Type where
  ||| Construct a physical device.
  |||
  ||| @ty The type of the device.
  ||| @ident The identity of the device.
  ||| @es The execution environments hosted on the device.
  ||| @as A possible list of tags describing details over the device.
  MkDevice : (ty : DeviceTy)
           -> (ident : String)
           -> (es : Envs)
           -> (as : Maybe Attributes)
           -> Device

Devices : Type
Devices = List Device

-- --------------------------------------------------------------- [ Relations ]

||| Describes how different devices, and different execution environments, talk to each other.
data Relation : Type where
  ||| Construct a relation between two nodes.
  |||
  ||| @nodeA A device.
  ||| @nodeB The other device being communicated with.
  ||| @protocol The protocol being used for communication.
  MkRelation : (nodeA : String)
            -> (nodeB : String)
            -> (protocol : String)
            -> Relation

Relations : Type
Relations = List Relation

||| A model for UML Deployment Diagrams.
data DeploymentModel : Type where
  ||| Construct a Deployment Diagram
  |||
  ||| @ds The physical devices.
  ||| @rs The relations between the devices.
  MkDeployment : (ds : Devices)
               -> (rs : Relations)
               -> DeploymentModel

-- ---------------------------------------------------------------------- [ Eq ]

instance Eq DeviceTy where
  (==) GenericDev   GenericDev  = True
  (==) Server       Server      = True
  (==) Workstation  Workstation = True
  (==) Mobile       Mobile      = True
  (==) Embedded     Embedded    = True
  (==) _            _           = False

instance Eq EnvTy where
  (==) OS         OS        = True
  (==) Engine     Engine    = True
  (==) Container  Container = True
  (==) AppServer  AppServer = True
  (==) App        App       = True
  (==) _          _         = False

instance Eq ArtifactTy where
  (==) Document     Document   = True
  (==) Source       Source     = True
  (==) Library      Library    = True
  (==) Executable   Executable = True
  (==) Script       Script     = True
  (==) _            _          = False

instance Eq Specification where
  (==) (MkSpec x xs) (MkSpec y ys) = x == y && xs == ys

instance Eq Artifact where
  (==) (MkArtifact xTy x xs) (MkArtifact yTy y ys) = xTy == yTy && x == y && xs == ys

instance Eq Env where
  (==) (MkEnv xTy x xs xxs) (MkEnv yTy y ys yys) = xTy == yTy && x == y && xs == ys && xxs == yys

instance Eq Device where
  (==) (MkDevice xTy x xs xxs) (MkDevice yTy y ys yys) = xTy == yTy && x == y && xs == ys && xxs == yys

instance Eq Relation where
  (==) (MkRelation xa xb xp) (MkRelation ya yb yp) = xa == ya && xb == yb && xp == yp

instance Eq DeploymentModel where
  (==) (MkDeployment xds xrs) (MkDeployment yds yrs) = xds == yds && xrs == yrs

-- -------------------------------------------------------------------- [ Show ]

instance Show DeviceTy where
  show GenericDev  = "Generic"
  show Server      = "Server"
  show Workstation = "Workstation"
  show Mobile      = "Mobile"
  show Embedded    = "Embedded"

instance Show EnvTy where
  show OS        = "OS"
  show Engine    = "Engine"
  show Container = "Container"
  show AppServer = "AppServer"
  show App       = "App"

instance Show ArtifactTy where
  show Document   = "Document"
  show Source     = "Source"
  show Library    = "Library"
  show Executable = "Executable"
  show Script     = "Script"

instance Show Specification where
  show (MkSpec id as) = unwords
       ["[Specification", show id, show as, "]"]

instance Show Artifact where
  show (MkArtifact ty id spec) = unwords
       ["[Artifact", show ty, show id, show spec, "]"]

instance Show Env where
  show (MkEnv ty id as ps) = unwords
       ["[Env", show ty, show id, show as, show ps, "]"]

instance Show Device where
  show (MkDevice ty id es as) = unwords
       ["[Device", show ty, show id, show es, show as, "]"]

instance Show Relation where
  show (MkRelation xID yID proto) = "[Relation " ++ xID ++ " " ++ yID ++ " " ++ proto ++ "]"

instance Show DeploymentModel where
  show (MkDeployment ds rs) = unwords
       ["[DeployModel", show ds, show rs, "]"]

-- --------------------------------------------------------------------- [ EOF ]
