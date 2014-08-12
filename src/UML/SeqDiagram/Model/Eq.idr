module UML.SeqDiagram.Model.Eq

import UML.SeqDiagram.Model

instance Eq MessageStep where
  (==) (MkStep fx tx xs) (MkStep fy ty ys) = fx == fy && tx == ty && xs == ys
