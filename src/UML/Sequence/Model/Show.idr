-- ---------------------------------------------------------------- [ Show.idr ]
-- Module      : UML.Sequence.Model.Show
-- Description : Show implementation.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Sequence.Model.Show

import UML.Sequence.Model

instance Show MessageStep where
  show (MkStep f t ms) = unwords
       [show f, "->", show t, "[", show ms, "]"]

-- --------------------------------------------------------------------- [ EOF ]
