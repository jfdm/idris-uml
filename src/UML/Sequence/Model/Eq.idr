module UML.Sequence.Model.Eq

import UML.Sequence.Model

instance Eq MessageStep where
  (==) (MkStep fx tx xs) (MkStep fy ty ys) = fx == fy && tx == ty && xs == ys
