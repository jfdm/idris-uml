-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Class.Model
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.ClassDiagram.Model

data Modifier = Abstract | Static
data Visibility = Private | Protected | Package | Public

data RelationTy = Specialisation
                | Aggregation
                | Composition
                | Association
                | Realisation

data ClassTy = ClassAbstract
             | ClassStandard
             | ClassInterface

-- ----------------------------------------------------------------- [ Methods ]

data Param : Type where
    MkParam : (id : String) -> (ty : String) -> Param

Params : Type
Params = List Param

-- ----------------------------------------------------------------- [ Methods ]
data Method : Type where
    MkMethod : (id : String)
             -> (retTy : String)
             -> Maybe Modifier
             -> Visibility
             -> Maybe Params
             -> Method

Methods : Type
Methods = List Method

-- -------------------------------------------------------------- [ Attributes ]

data Attribute : Type where
    MkAttribute : (id : String)
                -> (type : String)
                -> Maybe Modifier
                -> Visibility
                -> Attribute

Attributes : Type
Attributes = List Attribute

-- ----------------------------------------------------------------- [ Classes ]

data Class : Type where
    MkClass : (id : String)
            -> (ty : ClassTy)
            -> Maybe Attributes
            -> Maybe Methods
            -> Class

Classes : Type
Classes = List Class

-- --------------------------------------------------------------- [ Relations ]

data Relation : Type where
    MkRelation : RelationTy -> String -> String -> Maybe String -> Relation

Relations : Type
Relations = List Relation

-- ------------------------------------------------------------ [ ClassDiagram ]

data ClassDiagram : Type where
    MkClassDiagram : Classes -> Relations -> ClassDiagram


-- ---------------------------------------------------------------------- [ Eq ]

instance Eq Modifier where
    (==) Abstract Abstract = True
    (==) Static   Static   = True
    (==) _        _        = False

instance Eq Visibility where
    (==) Private   Private   = True
    (==) Protected Protected = True
    (==) Package   Package   = True
    (==) Public    Public    = True
    (==) _         _         = False

instance Eq RelationTy where
    (==) Specialiastion Specialiastion = True
    (==) Aggregation    Aggregation    = True
    (==) Composition    Composition    = True
    (==) Association    Association    = True
    (==) Realisation    Realisation    = True
    (==) _              _              = False

instance Eq ClassTy where
    (==) ClassAbstract  ClassAbstract  = True
    (==) ClassStandard  ClassStandard  = True
    (==) ClassInterface ClassInterface = True
    (==) _              _              = False

instance Eq Param where
    (==) (MkParam x xty) (MkParam y yty) = x == y && xty == yty

instance Eq Method where
    (==) (MkMethod x xrTy xm xv xs) (MkMethod y yrTy ym yv ys) =
         x == y && xrTy == yrTy && xm == ym && xv == yv && xs == ys

instance Eq Attribute where
    (==) (MkAttribute x xty xm xv) (MkAttribute y yty ym yv) =
         x == y && xty == yty && xm == ym && xv == yv

instance Eq Class where
    (==) (MkClass x xty xas xms) (MkClass y yty yas yms) =
         x == y && xty == yty && xas == yas && xms == yms

instance Eq Relation where
    (==) (MkRelation xty xa xb xs) (MkRelation yty ya yb ys) =
         xty == yty && xa == ya && xb == yb && xs == ys

instance Eq ClassDiagram where
    (==) (MkClassDiagram xcs xrs) (MkClassDiagram ycs yrs) =
         xcs == ycs && yrs == xrs

-- -------------------------------------------------------------------- [ Show ]

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
         ["[Class", show cs, show rs, "]"]

-- --------------------------------------------------------------------- [ EOF ]
