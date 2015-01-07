module Main

import System

import TestRunner
import ParsingTest

import UML
import UML.Reader

-- --------------------------------------------------------------------- [ XML ]


classTest : Test
classTest = parseTestGood classModel "Class Bob {\n + ad : String\n}\n\n Class Alice\n\n Bob --> Alice"

seqTest : Test
seqTest = parseTestGood sequenceModel "A -> B : Syn\nB -> A : Ack\n\nA -> B : Syn, Ack"

depTest : Test
depTest = parseTestGood deploymentModel "device server presentation {\n    env appserver presentation {\n        artifact exe policyAdminUIwar with spec specxml {\n            a : \"asas\",\n            b : \"asas\"\n        }\n    }\n    properties {\n        a : \"sdsd\",\n        b : \"asas\"\n    }\n}\n\ndevice server policy {\n    env appserver policy {\n        artifact exe policyServerjar with spec specxml\n        properties {\n            a : \"sdsd\",\n            b : \"asas\"\n        }\n    }\n}\n\ndevice server product {\n    env appserver product {\n        artifact exe productServerjar\n    }\n    env engine rules {\n        artifact doc rules\n    }\n}\n\ndevice workstation user {\n    env os workstation {\n        artifact exe productServerUIexe\n    }\n}\n\ndevice server documents {\n    env os server {\n        artifact exe docMngmntSystem\n    }\n}\n\ndevice server dbServer {\n    env engine dbms {\n        artifact doc schemaproduct\n        artifact doc schemapolicy\n    }\n}\n\ndevice server dirServer {\n    env app ldapServer {\n        artifact doc GrpUsrheirarchy\n    }\n}\n\ndevice server uWriteRating {\n    env engine rules {\n        artifact doc ratingrules\n        artifact doc underwritingrules\n    }\n    env appserver apps {\n        artifact exe underwritingenginejar\n        artifact exe ratingenginejar\n    }\n}\n\npresentation commsWith policy via http\npolicy commsWith uWriteRating via http\npolicy commsWith dirServer via http\npolicy commsWith dbServer via http\npolicy commsWith documents via http\ndbServer commsWith product via http\nproduct commsWith documents via http\nuser commsWith product via http\n"

compTest : Test
compTest = parseTestGood componentModel "data Int\ndata String\n\ndata User {\n  id : String,\n  name : String\n}\n\ndata Item {\n  id : String,\n  name : String,\n  desc : String\n}\n\ndata Order {\n  id : String,\n  item : Items\n}\n\ncomponent Accounting {\n  provides ManageOrders from Orders\n  provides ManageCustomers from Customers\n  requires ManageInventory from Warehouses\n\n  component Customers {\n    interface ManageCustomers {\n      addCustomer : (c : Customer) -> Bool\n      removeCustomer : (c : Customer) -> Bool\n    }\n  }\n\n  component Orders {\n    interface ManageOrders {\n      createOrder : Int\n      addToOrder : (p : Product) -> (orderID : Int) -> Bool\n      removeFromOrder : (p : Product) -> (orderID : Int) -> Bool\n    }\n    requires ManageCustomers from Customers\n    requires ManageInventory from Warehouses\n  }\n}\n\ncomponent Warehouses {\n  provides SearchInventory from Inventory\n  provides ManageInventory from Inventory\n\n  component Inventory {\n    interface SearchInventory {\n      createOrder : Int\n      addToOrder : (p : Product) -> (orderID : Int) -> Bool\n      removeFromOrder : (p : Product) -> (orderID : Int) -> Bool\n    }\n    interface ManageInventory {\n      createOrder : Int\n      addToOrder : (p : Product) -> (orderID : Int) -> Bool\n      removeFromOrder : (p : Product) -> (orderID : Int) -> Bool\n    }\n  }\n}\n\ncomponent WebStore {\n  provides ProductSearch from SearchEngine\n  provides OnlineShopping from ShoppingCart\n  provides UserSession from Authentication\n\n  requires SearchInventory from Warehouses\n  requires ManageOrders from Accounting\n  requires ManageCustomers from Accounting\n\n  component Authentication {\n    interface UserSession {\n      createOrder : Int\n      addToOrder : (p : Product) -> (orderID : Int) -> Bool\n      removeFromOrder : (p : Product) -> (orderID : Int) -> Bool\n    }\n    requires ManageCustomers from Accounting\n  }\n\n  component ShoppingCart {\n    interface OnlineShopping {\n      createOrder : Int\n      addToOrder : (p : Product) -> (orderID : Int) -> Bool\n      removeFromOrder : (p : Product) -> (orderID : Int) -> Bool\n    }\n    requires UserSession from Authentication\n    requires ManageOrders from Accounting\n  }\n\n  component SearchEngine {\n    interface ProductSearch {\n      createOrder : Int\n      addToOrder : (p : Product) -> (orderID : Int) -> Bool\n      removeFromOrder : (p : Product) -> (orderID : Int) -> Bool\n\n    }\n    requires SearchInventory from Warehouses\n  }\n\n}\n"

-- -------------------------------------------------------------------- [ Main ]
main : IO ()
main = do
    run $ tests (Z) [classTest, seqTest, depTest, compTest]
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
