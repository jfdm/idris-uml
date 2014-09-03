module UML.Deployment.Model.Show

import UML.Deployment.Model

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
  show (MkRelation xID yID proto) = unwords
       ["[Relation", show xID, show yID, show proto, "]"]

instance Show DeploymentDiagram where
  show (MkDeployment ds rs) = unwords
       ["[DeployDia", show ds, show rs, "]"]

-- --------------------------------------------------------------------- [ EOF ]
