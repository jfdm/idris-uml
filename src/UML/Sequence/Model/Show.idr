module UML.Sequence.Model.Show

import UML.Sequence.Model

instance Show MessageStep where
  show (MkStep f t ms) = unwords
       [show f, "->", show t, "[", show ms, "]"]
