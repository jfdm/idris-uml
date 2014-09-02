module UML.DeployDiagram.Model.Show

import UML.DeployDiagram.Model

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

instance Show Device where
  show (MkDevice ty id as) = unwords ["Device", show ty, show id, show as]

instance Show Env where
  show (MkEnv ty id as) = unwords ["Env", show ty, show id, show as]

instance Show Artifact where
  show (MkArtifact ty id) = unwords ["Artifact", show ty, show id]

instance Show Specification where
  show (MkSpec id as) = unwords ["Specfification", show id, show as]

instance Show Relation where
  show (AssignSpec sID artID) = unwords ["AssignSpec", show sID, show artID]
  show (Hosts dID eID) = unwords ["Hosts", show dID, show eID]
  show (Runs eID aID) = unwords ["Runs", eID, aID]
  show (Comms xID yID proto) = unwords ["Comms", show xID, show yID, show proto]

instance Show DeployDiagram where
  show (MkDeployDiagram ds es as ss es) = unwords ["DeployDia",
       show ds, show es, show as, show ss, show es]

-- --------------------------------------------------------------------- [ EOF ]
