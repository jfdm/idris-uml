module UML.Component.Model


data DType : Type where
    MkSType : (name : String) -> DType
    MkCType : (name : String) -> (attrs : List (String, String)) -> DType
--    MkLType : (name : String) -> (itemTy : DType) -> DType

DTypes : Type
DTypes = List DType

||| Defines a function in an interface.
data Function : Type where
    ||| Constructs a new function.
    MkPFunc : (name : String)
            -> (ps : List (String, String))
            -> (retTy : String) -> Function
    MkFunc : (name : String)
           -> (rety : String)
           -> Function

||| Data type to represent interfaces between components.
data Interface : Type where
    ||| Constructs an interface
    Actual : (name : String) -> (fns : List Function) -> Interface
    ||| Construct a provided interface.
    Provided : (name : String) -> (origin : String) -> Interface
    ||| Construct a required interface.
    Required : (name : String) -> (origin : String) -> Interface

Interfaces : Type
Interfaces = List Interface

||| A component in the diagram.
data Component : Type where
    ||| Constructs a new component.
    |||
    ||| @name The components name.
    ||| @ps A list of provided interfaces.
    ||| @as A list of actual interfaces.
    ||| @rs A list of required interfaces.
    ||| @cs A list of possible sub components.
    MkComponent : (name : String)
                -> (ps : Maybe Interfaces)
                -> (as : Maybe Interfaces)
                -> (rs : Maybe Interfaces)
                -> (cs : Maybe (List Component))
                -> Component

Components : Type
Components = List Component

||| A Component diagram is just a list of components.
data ComponentDiagram : Type where
    MkComponentDiagram : (ds : DTypes)
                       -> (cs : Components)
                       -> ComponentDiagram

-- --------------------------------------------------------------------- [ EOF ]
