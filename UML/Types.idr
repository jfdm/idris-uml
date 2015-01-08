module UML.Types

import UML.Class.Model
import UML.Sequence.Model
import UML.Deployment.Model
import UML.Component.Model

data UMLTy = CLAZZ | SEQUENCE | DEPLOYMENT | COMPONENT

data UML : Type where
  Class      : ClassModel MODEL -> UML
  Component  : ComponentModel   -> UML
  Sequence   : SequenceModel MODEL -> UML
  Deployment : DeploymentModel  -> UML


instance Show UMLTy where
  show CLAZZ      = "Clazz"
  show SEQUENCE   = "Sequence"
  show DEPLOYMENT = "Deployment"
  show COMPONENT  = "Component"

instance Eq UMLTy where
  (==) CLAZZ      CLAZZ      = True
  (==) SEQUENCE   SEQUENCE   = True
  (==) DEPLOYMENT DEPLOYMENT = True
  (==) COMPONENT  COMPONENT  = True
  (==) _          _          = False

-- --------------------------------------------------------------------- [ EOF ]
