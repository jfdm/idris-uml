module UML.Deployment.Model.Eq

import UML.Deployment.Model

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
  (==) (MkRelation xa xb xp) (MkRelation ya yb yp) =
       xa == ya && xb == yb && xp == yp
  (==) _                      _                    = False

instance Eq DeploymentDiagram where
  (==) (MkDeployment xds xrs) (MkDeployment yds yrs) =
       xds == yds && xrs == yrs
-- --------------------------------------------------------------------- [ EOF ]
