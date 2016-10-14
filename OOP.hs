

-- OOP paradigm in Haskell

{-# LANGUAGE TemplateHaskell #-}

import Control.Lens as Lens

class Class c where
    (->>) :: c -> Getting t c t -> t
    object ->> accessor = Lens.view accessor object
    
    (-->>) :: c -> ASetter c t a b -> b -> t
    object -->> accessor = \value -> Lens.over accessor (\_ -> value) object
  
data Person = Person {
    _name :: String,
    _age :: Int,
    
    _sayHiTo :: Person -> String
}
Lens.makeLenses ''Person
instance Class Person

-- Person john with default name
john = Person
    ""
    30
    (\person -> "Hello " ++ (person->>name) ++ "\n")

-- Person smith with default name    
smith = Person
    ""
    29
    (\person -> "Hello " ++ (person->>name) ++ "\n")

testClasses :: IO ()
testClasses = putStr $ newJohn ->> sayHiTo $ newSmith where
    newJohn = john -->> name $ "john"
    newSmith = smith -->> name $ "smith"