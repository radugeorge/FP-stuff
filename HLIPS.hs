

{- Haskell Language Integrated Production System -}

-------------------------
-- Language extentions --
-------------------------
{-# LANGUAGE ViewPatterns, ExistentialQuantification, ScopedTypeVariables #-}

import Control.Monad.State as StateM

-----------------------------
-- Fact concept definition --
-----------------------------
data FactItem = FactString                                  String 
              | FactDouble                                  Double 
              | FactInt                                     Int 
              | FactInteger                                 Integer 
              | FactBool                                    Bool 
              | FactListItem                                [FactItem]
              | forall a. (Show a,Eq a,Ord a) => FactList   [a]

instance Show FactItem where
    show (FactString ent) 
        = ent
    show (FactDouble ent) 
        = show ent
    show (FactInt ent) 
        = show ent
    show (FactInteger ent) 
        = show ent
    show (FactBool ent) 
        = show ent
    show (FactListItem ent)
        = show ent
    show (FactList ent)
        = show ent

data Fact = Fact {index::Int, fact::[FactItem]}
instance Show Fact where
    show (Fact index factItems) = show index ++ " <- " ++ show factItems

data FactBase = FactBase [Fact]
instance Show FactBase where
    show (FactBase [singularFact]) = show singularFact ++ "\nFor a total of 1 fact"
    show factBase = show' 0 factBase where
                        show' total (FactBase []) = "For a total of " ++ (show total) ++ " facts" 
                        show' total (FactBase (fact : facts)) = show fact ++ "\n" ++ show' (total+1) (FactBase facts)


assertFact :: FactBase -> [FactItem] -> Int -> FactBase
assertFact (FactBase fDb) factItems index = FactBase (fDb ++ [Fact index factItems])

retractFact :: FactBase -> Int -> FactBase
retractFact factBase index = retractFact' [] factBase index where
    retractFact' acc (FactBase []) _ = FactBase acc
    retractFact' acc (FactBase (fact@(Fact factIndex _) : facts)) index
        | index == factIndex = FactBase(acc ++ facts)
        | otherwise = retractFact' (acc ++ [fact]) (FactBase facts) index

defFacts :: FactBase -> [[FactItem]] -> Int -> FactBase
defFacts fDb [] _ = fDb
defFacts fDb (fact : facts) index = defFacts (assertFact fDb fact index) facts $ index + 1

-----------------------------
-- Rule concept definition --
-----------------------------
data Rule = Rule String ([Fact] -> CompiledRule)
newtype RuleBase = RuleBase [Rule]

data ExecutedRule = ExecutedRule (String,[Int]) deriving (Eq, Show)
newtype ExecutedRules = ExecutedRules [ExecutedRule]

type CompiledRule = [(ExecutedRule, [State InfEngine ()])]

defRule :: RuleBase -> String -> (String -> [Fact] -> [(ExecutedRule, [State InfEngine ()])]) -> RuleBase
defRule (RuleBase ruleBase) ruleName ruleBody = RuleBase $ ruleBase ++ [Rule ruleName $ ruleBody ruleName]

-------------------------------
-- Engine concept definition --
-------------------------------
data InfEngine = InfEngineItem (FactBase, RuleBase, ExecutedRules, Int)

instance Show InfEngine where
    show (InfEngineItem (factBase, _, _, _))
       = show factBase

assert :: [FactItem] -> State InfEngine ()
assert fact = do
   (InfEngineItem (factBase, ruleBase, executedRules, currentIndex)) <- get
   put $ InfEngineItem (assertFact factBase fact (currentIndex + 1), ruleBase, executedRules, currentIndex+1)
        
retract :: Int -> State InfEngine ()
retract index = do
   (InfEngineItem (factBase, ruleBase, executedRules, currentIndex)) <- get
   put $ InfEngineItem (retractFact factBase index, ruleBase, executedRules, currentIndex)
        
deffacts :: [[FactItem]] -> State InfEngine ()
deffacts facts = do
   (InfEngineItem (factBase, ruleBase, executedRules, currentIndex)) <- get
   put $ InfEngineItem (defFacts factBase facts (currentIndex + 1), ruleBase, executedRules, currentIndex)

defrule :: String -> (String -> [Fact] -> [(ExecutedRule, [State InfEngine ()])]) -> State InfEngine ()
defrule ruleName ruleBody = do
    (InfEngineItem (factBase, ruleBase, executedRules, currentIndex)) <- get
    put $ InfEngineItem (factBase, defRule ruleBase ruleName ruleBody, executedRules, currentIndex)

nop :: State InfEngine ()
nop = do
    (InfEngineItem infEngine) <- get
    put $ InfEngineItem (infEngine)

(==>) :: String -> [Fact] -> [State InfEngine ()] -> CompiledRule
(==>) ruleName facts ruleActions = do
   return (ExecutedRule (ruleName, (map (\fact -> index fact) facts)), ruleActions)
   
executeProgramHLIPS :: State InfEngine ()
executeProgramHLIPS = do 
   (InfEngineItem ((FactBase facts) , (RuleBase rules) , _, _)) <- get
   let compiledRules = extractCompiledRules rules
   exit <- executeCompiledRules (compileRules compiledRules facts)
   if exit
      then do 
         nop
      else do
         executeProgramHLIPS

executeCompiledRules :: CompiledRule -> State InfEngine Bool
executeCompiledRules [] = do 
   nop
   return True
   
executeCompiledRules ((compiledRule , body): xs) = do
   (InfEngineItem (factBase , ruleBase , (ExecutedRules executedRules), currentIndex)) <- get
   let result = checkForRefraction executedRules compiledRule
   if result /= []
   then do 
      executeCompiledRules xs
   else do
      put $ InfEngineItem (factBase , ruleBase , (ExecutedRules (executedRules ++ [compiledRule])), currentIndex)
      executeCompiledRules' body
      return False
       where
         executeCompiledRules' [] = nop
         executeCompiledRules' (y:ys) = do
            y      
            executeCompiledRules' ys
         
extractCompiledRules :: [Rule] -> [([Fact] -> CompiledRule)]
extractCompiledRules [] = []
extractCompiledRules ((Rule _ action) : rules) = [action] ++ (extractCompiledRules rules)

compileRules :: [([Fact] -> CompiledRule)] -> [Fact] -> CompiledRule
compileRules [] _ = []
compileRules (action : actions) facts = (action facts) ++ (compileRules actions facts)

checkForRefraction :: [ExecutedRule] -> ExecutedRule -> [ExecutedRule]
checkForRefraction executedRules executedRule = do
   result@((== executedRule) -> True) <- executedRules
   return result

runInfEngine :: State InfEngine () -> State InfEngine InfEngine
runInfEngine defineProgramHLIPS = do 
--------------------------------------------------
   defineProgramHLIPS
   executeProgramHLIPS
--------------------------------------------------
   infEngineFinalState

infEngineFinalState :: State InfEngine InfEngine
infEngineFinalState = get

---------------------------------------
-- Initial state of inference engine --
--------------------------------------- 
initialInfEngineState = InfEngineItem (FactBase [Fact 0 [FactString "initial-fact"]], RuleBase [], ExecutedRules [], 0)

computeInfState :: State InfEngine () -> InfEngine
computeInfState programHLIPSBody = evalState (runInfEngine programHLIPSBody) initialInfEngineState

--------------------------------------
-- Test engine - factorial function --
--------------------------------------
computeFactorial name facts = do
    factFactorial @(Fact index [FactString "factorial", FactInt n@((> 1) -> True)])  <- facts
    factRes       @(Fact indexRes [FactString "result", FactInt res])                <- facts
    (==>) name [factFactorial, factRes]
          [ retract index,
            retract indexRes,
            assert [FactString "factorial", FactInt $ n-1],
            assert [FactString "result", FactInt $ res * n]]
            
finishFactorial name facts = do
    factFactorial @(Fact index [FactString "factorial", FactInt 1])    <- facts
    factRes       @(Fact indexRes [FactString "result", FactInt res])  <- facts
    (==>) name [factFactorial, factRes]
          [ retract index,
            retract indexRes,
            assert [FactString "factorial is", FactInt res]]

factorial n = do
    assert [FactString "factorial", FactInt n]
    assert [FactString "result", FactInt 1]
    defrule "compute-factorial" computeFactorial
    defrule "finish-factorial" finishFactorial

factorialOf n = computeInfState $ factorial n

{- Result:
*Main> factorialOf 5
0 <- [initial-fact]
11 <- [factorial is,120]
For a total of 2 facts
*Main> 
-}