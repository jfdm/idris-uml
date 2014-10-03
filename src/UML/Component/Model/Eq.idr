module UML.Component.Model.Eq

import UML.Component.Model

%default partial
%access public


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

-- --------------------------------------------------------------------- [ EOF ]
