module UML.Component.Model.Eq

import UML.Component.Model

%default partial
%access public

instance Eq Interface where
    (==) (Provides xn xo) (Provides yn yo) = xn == yn && xo == yo
    (==) (Requires xn xo) (Requires yn yo) = xn == yn && xo == yo
    (==) _ _ = False


mutual
  private
  %assert_total
  componentEq : Component -> Component -> Bool
  componentEq (MkComponent x xps xrs xcs) (MkComponent y yps yrs ycs) =
           x == y && xps == yps && xrs == yrs && xcs == ycs
  componentEq _ _ = False

  instance Eq Component where
      (==) x y = componentEq x y
-- --------------------------------------------------------------------- [ EOF ]
