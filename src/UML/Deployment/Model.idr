module UML.Deployment.Model

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
data DeploymentDiagram : Type where
  ||| Construct a Deployment Diagram
  |||
  ||| @ds The physical devices.
  ||| @rs The relations between the devices.
  MkDeployment : (ds : Devices)
               -> (rs : Relations)
               -> DeploymentDiagram
-- --------------------------------------------------------------------- [ EOF ]
