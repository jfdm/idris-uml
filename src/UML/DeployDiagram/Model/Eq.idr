module UML.DeployDiagram.Model.Eq

import UML.DeployDiagram.Model

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

instance Eq Device where
  (==) (MkDevice xTy x xs) (MkDevice yTy y ys) = xTy == yTy && x == y && xs == ys

instance Eq Env where
  (==) (MkEnv xTy x xs) (MkEnv yTy y ys) = xTy == yTy && x == y && xs == ys

instance Eq Artifact where
  (==) (MkArtifact xTy x) (MkArtifact yTy y) = xTy == yTy && x == y

instance Eq Specification where
  (==) (MkSpec x xs) (MkSpec y ys) = x == y && xs == ys

instance Eq Relation where
  (==) (AssignSpec xa xb) (AssignSpec ya yb) = xa == ya && xb == yb
  (==) (Hosts xa xb)      (Hosts ya yb)      = xa == ya && xb == yb
  (==) (Runs xa xb)       (Runs ya yb)       = xa == ya && xb == yb
  (==) (Comms xa xb xp)   (Comms ya yb yp)   = xa == ya && xb == yb && xp == yp
  (==) _                  _                  = False

instance Eq DeployDiagram where
  (==) (MkDeployDiagram xd xe xa xs xr) (MkDeployDiagram yd ye ya ys yr) =
       xd == yd && xe == ye && xa == ya && xs == ys && xr == yr
-- --------------------------------------------------------------------- [ EOF ]
