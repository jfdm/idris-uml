-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Component.Model
-- Description :
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Component.Model

%access public

data DType : Type where
    MkSType : (name : String) -> DType
    MkCType : (name : String) -> (attrs : List (String, String)) -> DType
--    MkLType : (name : String) -> (itemTy : DType) -> DType

DTypes : Type
DTypes = List DType

||| Defines a function in an interface.
data Function : Type where
    ||| Constructs a new function.
    MkPFunc : (name : String)
            -> (ps : List (String, String))
            -> (retTy : String) -> Function
    MkFunc : (name : String)
           -> (rety : String)
           -> Function

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
data ComponentDiagram : Type where
    MkComponentDiagram : (ds : DTypes)
                       -> (cs : Components)
                       -> ComponentDiagram

-- ------------------------------------------------------------------ [ Equals ]

instance Eq DType where
    (==) (MkSType x)    (MkSType y)    = x == y
    (==) (MkCType x xs) (MkCType y ys) = x == y && xs == ys
    (==) _ _ = False

instance Eq Function where
    (==) (MkPFunc x xs xr) (MkPFunc y ys yr) = x == y && xs == ys && xr == yr
    (==) (MkFunc x xr)     (MkFunc y yr)     = x == y && xr == yr
    (==) _ _ = False

instance Eq Interface where
    (==) (Provided xn xo) (Provided yn yo) = xn == yn && xo == yo
    (==) (Required xn xo) (Required yn yo) = xn == yn && xo == yo
    (==) (Actual xn xf)   (Actual yn yf)   = xn == yn && xf == yf
    (==) _ _ = False


mutual
  private
  %assert_total
  componentEq : Component -> Component -> Bool
  componentEq (MkComponent x xis xcs) (MkComponent y yis ycs) =
           x == y &&
           xis == yis &&
           xcs == ycs
  componentEq _ _ = False

  instance Eq Component where
      (==) x y = componentEq x y

instance Eq ComponentDiagram where
    (==) (MkComponentDiagram xd xc) (MkComponentDiagram yd yc) = xd == yd && xc == yc


-- -------------------------------------------------------------------- [ Show ]

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


-- --------------------------------------------------------------------- [ EOF ]
