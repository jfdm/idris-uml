-- ------------------------------------------------------------------ [ Eq.idr ]
-- Module      : UML.Sequence.Model.Eq
-- Description : Equality implementation for Sequence Diagrams
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Sequence.Model.Eq

import UML.Sequence.Model

instance Eq MessageStep where
  (==) (MkStep fx tx xs) (MkStep fy ty ys) = fx == fy && tx == ty && xs == ys
-- --------------------------------------------------------------------- [ EOF ]
