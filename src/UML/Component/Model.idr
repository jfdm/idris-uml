module UML.Component.Model

||| Data type to represent interfaces between components.
data Interface : Type where
    ||| Constructs a new provided interface.
    Provides : (name : String) -> (origin : Maybe String) -> Interface
    Requires : (name : String) -> (origin : String) -> Interface

Interfaces : Type
Interfaces = List Interface

||| A component in the diagram.
data Component : Type where
    ||| Constructs a new component.
    |||
    ||| @name The components name.
    ||| @ps A list of provided interfaces.
    ||| @rs A list of required interfaces.
    ||| @cs A list of possible sub components.
    MkComponent : (name : String)
                -> (ps : Interfaces)
                -> (rs : Maybe Interfaces)
                -> (cs : Maybe (List Component))
                -> Component

||| A Component diagram is just a list of components.
ComponentDiagram : Type
ComponentDiagram = List Component

-- --------------------------------------------------------------------- [ EOF ]
