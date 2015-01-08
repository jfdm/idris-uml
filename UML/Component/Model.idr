-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Component.Model
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Component.Model

import UML.Code.Model

%access public

data ElemTy = INTERFACE | COMPONENT | MODEL

data ComponentModel : ElemTy -> Type where
    ||| Constructs an interface
    Actual : (name : String) -> (fns : List Function) -> ComponentModel INTERFACE
    ||| Construct a provided interface.
    Provided : (name : String) -> (origin : String) -> ComponentModel INTERFACE
    ||| Construct a required interface.
    Required : (name : String) -> (origin : String) -> ComponentModel INTERFACE

    ||| A component in the diagram.
    |||
    ||| @name The components name.
    ||| @is A list of interfaces.
    ||| @cs A list of possible sub components.
    MkComponent : (name : String)
               -> (is : List $ ComponentModel INTERFACE)
               -> (cs : List $ ComponentModel COMPONENT)
               -> ComponentModel COMPONENT

    ||| A Component diagram is just a list of components.
    MkComponentModel : (ds : DTypes)
                     -> (cs : List $ ComponentModel COMPONENT)
                     -> ComponentModel MODEL

-- ------------------------------------------------------------------ [ Equals ]

instance Eq ElemTy where
  (==) INTERFACE INTERFACE = True
  (==) COMPONENT COMPONENT = True
  (==) MODEL     MODEL     = True
  (==) _         _         = False

mutual

  %assert_total
  eqCompModel : (ComponentModel x) -> (ComponentModel x) -> Bool
  eqCompModel (Provided xn xo)         (Provided yn yo)         = xn == yn && xo == yo
  eqCompModel (Required xn xo)         (Required yn yo)         = xn == yn && xo == yo
  eqCompModel (Actual xn xf)           (Actual yn yf)           = xn == yn && xf == yf
  eqCompModel (MkComponent x xis xcs)  (MkComponent y yis ycs)  =  x == y && xis == yis && xcs == ycs
  eqCompModel (MkComponentModel xd xc) (MkComponentModel yd yc) = xd == yd && xc == yc
  eqCompModel _                     _                           = False

  instance Eq (ComponentModel x) where
    (==) = eqCompModel


-- -------------------------------------------------------------------- [ Show ]

instance Show ElemTy where
  show INTERFACE = "INTERFACE"
  show COMPONENT = "COMPONENT"
  show MODEL     = "MODEL"

instance Show (ComponentModel x) where
  show (Actual n fs)            = unwords ["[Interface", show n, show fs, "]\n"]
  show (Provided n o)           = unwords ["[Provides Interface", show n, show o, "]\n"]
  show (Required n o)           = unwords ["[Requires Interface", show n, show o, "]\n"]
  show (MkComponent n is cs)    = unwords ["[Component", show n, "\n\t", show is, "\n\t", show cs,"]\n"]
  show (MkComponentModel ds cs) = unwords ["[CompModel", "\n", show ds, "\n", show cs, "\n]"]

-- --------------------------------------------------------------------- [ EOF ]
