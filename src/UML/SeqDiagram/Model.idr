module UML.SeqDiagram.Model

data MessageStep : Type where
    MkStep : (from : String) -> (to : String) -> (ms : List String) -> MessageStep

SeqDiagram : Type
SeqDiagram = List MessageStep
