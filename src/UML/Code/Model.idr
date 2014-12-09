-- --------------------------------------------------------- [ Model.idr<Code> ]
-- Module      : UML.Code.Model
-- Description : Data types for common code constructs.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Code.Model

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
            -> (ps : List (Pair String String))
            -> (retTy : String) -> Function
    MkFunc : (name : String)
           -> (rety : String)
           -> Function

-- ---------------------------------------------------------------------- [ Eq ]

instance Eq DType where
    (==) (MkSType x)    (MkSType y)    = x == y
    (==) (MkCType x xs) (MkCType y ys) = x == y && xs == ys
    (==) _ _ = False

instance Eq Function where
    (==) (MkPFunc x xs xr) (MkPFunc y ys yr) = x == y && xs == ys && xr == yr
    (==) (MkFunc x xr)     (MkFunc y yr)     = x == y && xr == yr
    (==) _ _ = False

-- -------------------------------------------------------------------- [ Show ]

instance Show DType where
    show (MkSType n) = unwords ["[Data Simple", show n, "]\n"]
    show (MkCType n as) = unwords ["[Data Complex", show n, show as, "]\n"]

instance Show Function where
    show (MkPFunc n ps rty) = unwords ["[Func", show n, show ps, show rty, " ]\n"]
    show (MkFunc n rty)     = unwords ["[Func", show n, show rty, "]\n"]

-- --------------------------------------------------------------------- [ EOF ]
