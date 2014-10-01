module UML.Component.Model.Show

import UML.Component.Model

instance Show DType where
    show (MkSType n) = unwords ["[Data Simple", show n, "]"]
    show (MkCType n as) = unwords ["[Data Complex", show n, show as, "]"]

instance Show Function where
    show (MkPFunc n ps rty) = unwords ["[Func", show n, show ps, show rty, " ]"]
    show (MkFunc n rty)     = unwords ["[Func", show n, show rty, "]"]

instance Show Interface where
    show (Actual n fs)  = unwords ["[Interface", show n, show fs, "]"]
    show (Provided n o) = unwords ["[Provides Interface", show n, show o, "]"]
    show (Required n o) = unwords ["[Requires Interface", show n, show o, "]"]

instance Show Component where
    show (MkComponent n ps as rs cs) = unwords ["[Component", show n,
         show ps, show as, show rs, show cs,"]"]

instance Show ComponentDiagram where
   show (MkComponentDiagram ds cs) = unwords ["[CompDiagram", show ds, show cs, "]"]
