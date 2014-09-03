module UML.Component.Model.Show

import UML.Component.Model

instance Show Interface where
    show (Provides n o) = unwords ["[Provides Interface", show n, show o, "]"]
    show (Requires n o) = unwords ["[Requires Interface", show n, show o, "]"]

instance Show Component where
    show (MkComponent n ps rs cs) = unwords ["[Component", show n,
         show ps, show rs, show cs,"]"]
