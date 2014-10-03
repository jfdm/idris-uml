module UML.ClassDiagram.Model

data Modifier = Abstract | Static
data Visibility = Private | Protected | Package | Public

data RelationTy = Specialisation
                | Aggregation
                | Composition
                | Association
                | Realisation

data ClassTy = ClassAbstract
             | ClassStandard
             | ClassInterface

-- ----------------------------------------------------------------- [ Methods ]

data Param : Type where
    MkParam : (id : String) -> (ty : String) -> Param

Params : Type
Params = List Param

-- ----------------------------------------------------------------- [ Methods ]
data Method : Type where
    MkMethod : (id : String)
             -> (retTy : String)
             -> Maybe Modifier
             -> Visibility
             -> Maybe Params
             -> Method

Methods : Type
Methods = List Method

-- -------------------------------------------------------------- [ Attributes ]

data Attribute : Type where
    MkAttribute : (id : String)
                -> (type : String)
                -> Maybe Modifier
                -> Visibility
                -> Attribute

Attributes : Type
Attributes = List Attribute

-- ----------------------------------------------------------------- [ Classes ]

data Class : Type where
    MkClass : (id : String)
            -> (ty : ClassTy)
            -> Maybe Attributes
            -> Maybe Methods
            -> Class

Classes : Type
Classes = List Class

-- --------------------------------------------------------------- [ Relations ]

data Relation : Type where
    MkRelation : RelationTy -> String -> String -> Maybe String -> Relation

Relations : Type
Relations = List Relation

-- ------------------------------------------------------------ [ ClassDiagram ]

data ClassDiagram : Type where
    MkClassDiagram : Classes -> Relations -> ClassDiagram
