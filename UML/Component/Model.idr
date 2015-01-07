-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Component.Model
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Component.Model

import UML.Code.Model

%access public

||| Data type to represent interfaces between components.
data Interface : Type where
    ||| Constructs an interface
    Actual : (name : String) -> (fns : List Function) -> Interface
    ||| Construct a provided interface.
    Provided : (name : String) -> (origin : String) -> Interface
    ||| Construct a required interface.
    Required : (name : String) -> (origin : String) -> Interface

Interfaces : Type
Interfaces = List Interface

||| A component in the diagram.
data Component : Type where
    ||| Constructs a new component.
    |||
    ||| @name The components name.
    ||| @is A list of interfaces.
    ||| @cs A list of possible sub components.
    MkComponent : (name : String)
                -> (is : Interfaces)
                -> (cs : Maybe (List Component))
                -> Component

Components : Type
Components = List Component

||| A Component diagram is just a list of components.
data ComponentModel : Type where
    MkComponentModel : (ds : DTypes)
                       -> (cs : Components)
                       -> ComponentModel

-- ------------------------------------------------------------------ [ Equals ]

instance Eq Interface where
    (==) (Provided xn xo) (Provided yn yo) = xn == yn && xo == yo
    (==) (Required xn xo) (Required yn yo) = xn == yn && xo == yo
    (==) (Actual xn xf)   (Actual yn yf)   = xn == yn && xf == yf
    (==) _ _ = False

mutual
  %assert_total
  componentEq : Component -> Component -> Bool
  componentEq (MkComponent x xis xcs) (MkComponent y yis ycs) = x == y && xis == yis && xcs == ycs

  instance Eq Component where
    (==) = componentEq

mutual
  %assert_total
  componentModelEq : ComponentModel -> ComponentModel -> Bool
  componentModelEq (MkComponentModel xd xc) (MkComponentModel yd yc) = xd == yd && xc == yc

  instance Eq ComponentModel where
      (==) = componentModelEq


-- -------------------------------------------------------------------- [ Show ]

instance Show Interface where
    show (Actual n fs)  = unwords ["[Interface", show n, show fs, "]\n"]
    show (Provided n o) = unwords ["[Provides Interface", show n, show o, "]\n"]
    show (Required n o) = unwords ["[Requires Interface", show n, show o, "]\n"]

instance Show Component where
    show (MkComponent n is cs) = unwords ["[Component",
         show n, "\n\t",
         show is, "\n\t",
         show cs,"]\n"]

instance Show ComponentModel where
   show (MkComponentModel ds cs) = unwords
        ["[CompModel", "\n",
        show ds, "\n",
        show cs, "\n]"]

-- --------------------------------------------------------------------- [ EOF ]
