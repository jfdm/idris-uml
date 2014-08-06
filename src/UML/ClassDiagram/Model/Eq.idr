module UML.ClassDiagram.Model.Eq

import UML.ClassDiagram.Model

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
