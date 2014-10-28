-- ---------------------------------------------------------------- [ Effs.idr ]
-- Module      : UML.Effs
-- Description : Common utils for effects.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Effs

import Effects
import Effect.StdIO
import Effect.System
import Effect.Exception
import Effect.File

%access public

||| Global list of effects.
UMLEffs : List EFFECT
UMLEffs = [STDIO, SYSTEM, EXCEPTION String, FILE_IO ()]
-- --------------------------------------------------------------------- [ EOF ]
