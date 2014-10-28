-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Sequence.Model
-- Description : A model for sequence diagrams.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Sequence.Model

%access public

||| A step in a sequence diagram.
data MessageStep : Type where
    ||| Construct a step.
    |||
    ||| @from The sending entity.
    ||| @to The receiving entity.
    ||| @ms The messages being sent.
    MkStep : (from : String) -> (to : String) -> (ms : List String) -> MessageStep

||| A diagram is a list of steps.
SequenceDiagram : Type
SequenceDiagram = List MessageStep

-- ---------------------------------------------------------------------- [ Eq ]

instance Eq MessageStep where
  (==) (MkStep fx tx xs) (MkStep fy ty ys) = fx == fy && tx == ty && xs == ys

-- -------------------------------------------------------------------- [ Show ]

instance Show MessageStep where
  show (MkStep f t ms) = unwords
       [show f, "->", show t, "[", show ms, "]"]

-- --------------------------------------------------------------------- [ EOF ]
