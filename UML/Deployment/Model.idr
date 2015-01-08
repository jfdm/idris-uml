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


data ElemTy = SPEC | ARTIFACT | ENV | DEVICE | RELA | MODEL

-- ------------------------------------------------------------------- [ Model ]

||| A model for UML Deployment Diagrams.
data DeploymentModel : ElemTy -> Type where
  ||| Description of a named deployment specification for artifacts.
  |||
  ||| @ident The name of the deployment specification,
  ||| @as The specification as KV pairs.
  Spec : (ident : String)
         -> (as : Attributes) -> DeploymentModel SPEC

  ||| Resources that are to be deployed in execution environments on devices.
  |||
  ||| @ty The type of artifact.
  ||| @ident The identify of the artifact.
  ||| @spec A possible specification schema that describes deployment.
  Artifact : (ty : ArtifactTy)
             -> (ident : String)
             -> (spec : Maybe (DeploymentModel SPEC))
             -> DeploymentModel ARTIFACT

  ||| A execution environment in which artifacts are run or hosted.
  |||
  ||| @ty The type of environment.
  ||| @ident The identity of the environment.
  ||| @as The artifacts being executed/hosted within the environment.
  ||| @ps A possible set of properties for the environment.
  Env : (ty : EnvTy)
      -> (ident : String)
      -> (as : List $ DeploymentModel ARTIFACT)
      -> (ps : Maybe Attributes)
      -> DeploymentModel ENV

  ||| A physical device in the deployment diagram.
  |||
  ||| @ty The type of the device.
  ||| @ident The identity of the device.
  ||| @es The execution environments hosted on the device.
  ||| @as A possible list of tags describing details over the device.
  Device : (ty : DeviceTy)
         -> (ident : String)
         -> (es : List $ DeploymentModel ENV)
         -> (as : Maybe Attributes)
         -> DeploymentModel DEVICE

  ||| Describes how different devices, and different execution environments, talk to each other.relation between two nodes.
  |||
  ||| @nodeA A device.
  ||| @nodeB The other device being communicated with.
  ||| @protocol The protocol being used for communication.
  Relation : (nodeA : String)
            -> (nodeB : String)
            -> (protocol : String)
            -> DeploymentModel RELA

  ||| A model for UML Deployment Diagrams.
  |||
  ||| @ds The physical devices.
  ||| @rs The relations between the devices.
  MkDeployment : (ds : List $ DeploymentModel DEVICE)
               -> (rs : List $ DeploymentModel $ RELA)
               -> DeploymentModel MODEL

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


mutual

  %assert_total
  eqDepModel : (DeploymentModel x) -> (DeploymentModel x) -> Bool
  eqDepModel (MkDeployment xds xrs) (MkDeployment yds yrs) = xds == yds && xrs == yrs
  eqDepModel (Spec x xs)            (Spec y ys)            = x == y && xs == ys
  eqDepModel (Artifact xTy x xs)    (Artifact yTy y ys)    = xTy == yTy && x == y && xs == ys
  eqDepModel (Env xTy x xs xxs)     (Env yTy y ys yys)     = xTy == yTy && x == y && xs == ys && xxs == yys
  eqDepModel (Device xTy x xs xxs)  (Device yTy y ys yys)  = xTy == yTy && x == y && xs == ys && xxs == yys
  eqDepModel (Relation xa xb xp)    (Relation ya yb yp)    = xa == ya && xb == yb && xp == yp
  eqDepModel _                      _                      = False

  instance Eq (DeploymentModel x) where
    (==) = eqDepModel
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

instance Show (DeploymentModel x) where
  show (MkDeployment ds rs)     = unwords ["[DeployModel", show ds, show rs, "]"]
  show (Spec id as)             = unwords ["[Specification", show id, show as, "]"]
  show (Artifact ty id spec)    = unwords ["[Artifact", show ty, show id, show spec, "]"]
  show (Env ty id as ps)        = unwords ["[Env", show ty, show id, show as, show ps, "]"]
  show (Device ty id es as)     = unwords ["[Device", show ty, show id, show es, show as, "]"]
  show (Relation xID yID proto) = "[Relation " ++ xID ++ " " ++ yID ++ " " ++ proto ++ "]"

-- --------------------------------------------------------------------- [ EOF ]
