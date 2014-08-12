module UML.SeqDiagram.Model.Show

import UML.SeqDiagram.Model

instance Show MessageStep where
  show (MkStep f t ms) = unwords
       [show f, "->", show t, "[", show ms, "]"]
