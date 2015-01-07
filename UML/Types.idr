module UML.Types

import UML.Class.Model
import UML.Sequence.Model
import UML.Deployment.Model
import UML.Component.Model

data UML : Type where
  Class      : ClassModel -> UML
  Component  : ComponentModel -> UML
  Sequence   : SequenceModel -> UML
  Deployment : DeploymentModel -> UML


-- --------------------------------------------------------------------- [ EOF ]
