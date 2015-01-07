-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Class.Model
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Class.Model

data Modifier = Abstract | Static
data Visibility = Private | Protected | Package | Public
data RelationTy = Specialisation | Aggregation | Composition | Association | Realisation
data ClassTy = ClassAbstract | ClassStandard | ClassInterface

-- ------------------------------------------------------------------- [ Model ]

data ElemTy = PARAM | METHOD | ATTR | CLASS | RELA | MODEL

data ClassModel : ElemTy -> Type where
  Param : String -> String -> ClassModel PARAM
  Method : String
         -> String
         -> Maybe Modifier
         -> Visibility
         -> List (ClassModel PARAM)
         -> ClassModel METHOD
  Attribute : String
            -> String
            -> Maybe Modifier
            -> Visibility
            -> ClassModel ATTR
  Clazz : String
        -> ClassTy
        -> List (ClassModel ATTR)
        -> List (ClassModel METHOD)
        -> ClassModel CLASS
  Relation : RelationTy
           -> String
           -> String
           -> Maybe String
           -> ClassModel RELA
  MkClassModel : List (ClassModel CLASS) -> List (ClassModel RELA) -> ClassModel MODEL

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
    (==) Specialisation Specialisation = True
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

instance Eq ElemTy where
    (==) PARAM  PARAM  = True
    (==) METHOD METHOD = True
    (==) ATTR   ATTR   = True
    (==) CLASS  CLASS  = True
    (==) RELA   RELA   = True
    (==) MODEL  MODEL  = True
    (==) _      _      = False


mutual
  %assert_total
  eqClassModel : ClassModel a -> ClassModel a -> Bool
  eqClassModel (Param x xty)            (Param y yty)            = x == y && xty == yty
  eqClassModel (Method x xrTy xm xv xs) (Method y yrTy ym yv ys) = x == y && xrTy == yrTy && xm == ym && xv == yv && xs == ys
  eqClassModel (Attribute x xty xm xv)  (Attribute y yty ym yv)  = x == y && xty == yty && xm == ym && xv == yv
  eqClassModel (Clazz x xty xas xms)    (Clazz y yty yas yms)    = x == y && xty == yty && xas == yas && xms == yms
  eqClassModel (Relation xty xa xb xs)  (Relation yty ya yb ys)  = xty == yty && xa == ya && xb == yb && xs == ys
  eqClassModel (MkClassModel xcs xrs)   (MkClassModel ycs yrs)   = xcs == ycs && yrs == xrs
  eqClassModel _                        _                        = False

  instance Eq (ClassModel x) where
      (==) = eqClassModel

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

instance Show ElemTy where
    show PARAM  = "PARAM"
    show METHOD = "METHOD"
    show ATTR   = "ATTR"
    show CLASS  = "CLASS"
    show RELA   = "RELA"
    show MODEL  = "MODEL"

instance Show (ClassModel x) where
    show (Param id ty)          = "[Param " ++ id ++ " : " ++ ty ++ "]"
    show (Method id rTy m v ps) = unwords ["[Method", id, ":", rTy, show m, show v, show ps, "]"]
    show (Attribute id ty m v)  = unwords ["[Attribute", id, ":", ty, show m, show v, "]"]
    show (Clazz id ty as ms)    = unwords ["[Class", id, ":", show ty, show as, show ms, "]"]
    show (Relation ty a b desc) = unwords ["[Relation", "(", show a, "->", show b, ")", ":", show ty, show desc, "]"]
    show (MkClassModel cs rs)   = unwords ["[Class", show cs, show rs, "]"]

-- --------------------------------------------------------------------- [ EOF ]
