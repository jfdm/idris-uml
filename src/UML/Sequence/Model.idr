module UML.Sequence.Model

data MessageStep : Type where
    MkStep : (from : String) -> (to : String) -> (ms : List String) -> MessageStep

SequenceDiagram : Type
SequenceDiagram = List MessageStep
