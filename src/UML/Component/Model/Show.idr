module UML.Component.Model.Show

import UML.Component.Model

instance Show DType where
    show (MkSType n) = unwords ["[Data Simple", show n, "]\n"]
    show (MkCType n as) = unwords ["[Data Complex", show n, show as, "]\n"]

instance Show Function where
    show (MkPFunc n ps rty) = unwords ["[Func", show n, show ps, show rty, " ]\n"]
    show (MkFunc n rty)     = unwords ["[Func", show n, show rty, "]\n"]

instance Show Interface where
    show (Actual n fs)  = unwords ["[Interface", show n, show fs, "]\n"]
    show (Provided n o) = unwords ["[Provides Interface", show n, show o, "]\n"]
    show (Required n o) = unwords ["[Requires Interface", show n, show o, "]\n"]

instance Show Component where
    show (MkComponent n is cs) = unwords ["[Component",
         show n, "\n\t",
         show is, "\n\t",
         show cs,"]\n"]

instance Show ComponentDiagram where
   show (MkComponentDiagram ds cs) = unwords
        ["[CompDiagram", "\n",
        show ds, "\n",
        show cs, "\n]"]
