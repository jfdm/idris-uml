module UML.Effs

import Effects
import Effect.StdIO
import Effect.System
import Effect.Exception
import Effect.File

UMLEffs : List EFFECT
UMLEffs = [STDIO, SYSTEM, EXCEPTION String, FILE_IO ()]
