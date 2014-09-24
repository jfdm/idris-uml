-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Sequence.Model
-- Description : A model for sequence diagrams.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Sequence.Model

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
-- --------------------------------------------------------------------- [ EOF ]
