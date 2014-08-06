module UML.ClassDiagram.Model.Show

import UML.ClassDiagram.Model

instance Show Modifier where
    show Abstract = "Abstract"
    show Static   = "Static"

instance Show Visibility where
    show Private   = "Private"
    show Protected = "Protected"
    show Package   = "Package"
    show Public    = "Public"

instance Show RelationTy where
    show Association    = "Association"
    show Specialiastion = "Specialisation"
    show Aggregation    = "Aggregation"
    show Composition    = "Composition"
    show Realisation    = "Realisation"

instance Show ClassTy where
    show ClassAbstract  = "ClassAbstract"
    show ClassStandard  = "ClassStandard"
    show ClassInterface = "ClassInterface"

instance Show Param where
    show (MkParam id ty) = "[Param " ++ id ++ " : " ++ ty ++ "]"

instance Show Method where
    show (MkMethod id rTy m v ps) = unwords
         ["[Method", id, ":", rTy, show m, show v, show ps, "]"]

instance Show Attribute where
    show (MkAttribute id ty m v) = unwords
         ["[Attribute", id, ":", ty, show m, show v, "]"]

instance Show Class where
    show (MkClass id ty as ms) = unwords
         ["[Class", id, ":", show ty, show as, show ms, "]"]

instance Show Relation where
    show (MkRelation ty a b desc) = unwords
         ["[Relation", "(", show a, "->", show b, ")", ":", show ty, show desc, "]"]

instance Show ClassDiagram where
    show (MkClassDiagram cs rs) = unwords
         ["[ClassDiagram", show cs, show rs, "]"]
