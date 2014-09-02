module UML.DeployDiagram.Model

data DeviceTy = GenericDev | Server | Workstation | Mobile | Embedded

-- ReThink
data EnvTy = OS | Engine | Container | AppServer | App

data ArtifactTy = Document | Source | Library | Executable | Script

Attribute : Type
Attribute = (String, String)

Attributes : Type
Attributes = List Attribute

-- ----------------------------------------------------------------- [ Devices ]

data Device : Type where
  MkDevice : DeviceTy
           -> String
           -> Maybe Attributes
           -> Device

Devices : Type
Devices = List Device

-- -------------------------------------------------------------------- [ Envs ]

data Env : Type where
  MkEnv : EnvTy
        -> String
        -> Maybe Attributes
        -> Env

Envs : Type
Envs = List Env

-- --------------------------------------------------------------- [ Artifacts ]

data Artifact : Type where
  MkArtifact : ArtifactTy
             -> String
             -> Artifact

Artifacts : Type
Artifacts = List Artifact

-- ---------------------------------------------------------- [ Specifications ]

data Specification : Type where
  MkSpec : String
         -> Attributes
         -> Specification

Specifications : Type
Specifications = List Specification

-- --------------------------------------------------------------- [ Relations ]

data Relation : Type where
  AssignSpec : String
             -> String
             -> Relation
  Hosts : String
        -> String
        -> Relation
  Runs : String
       -> String
       -> Relation
  Comms : String
        -> String
        -> String
        -> Relation

Relations : Type
Relations = List Relation

data DeployDiagram : Type where
  MkDeployDiagram : Devices
                  -> Envs
                  -> Artifacts
                  -> Maybe Specifications
                  -> Relations
                  -> DeployDiagram
-- --------------------------------------------------------------------- [ EOF ]
