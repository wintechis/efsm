module EFSM where

import Prelude

import Control.Monad.State (State, get, put)
import Data.Array (concatMap, find, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3)

data SymbolicState = SymbolicState String
derive instance eqSymbolicState :: Eq SymbolicState
derive instance ordSymbolicState :: Ord SymbolicState
instance showSymbolicState :: Show SymbolicState where
  show (SymbolicState state) = show state

data Input = Input String
derive instance eqInput :: Eq Input
derive instance ordInput :: Ord Input
instance showInput :: Show Input where
  show (Input inp) = show inp

data Output = Output String
derive instance eqOutput :: Eq Output
derive instance ordOutput :: Ord Output
instance showOutput :: Show Output where
  show (Output output) = show output

type Variables a = Record a

type EnablingFunction a = Variables a -> Boolean

type UpdateFunction a = Variables a -> Variables a

data Transition a = Transition SymbolicState (EnablingFunction a) (Maybe Input) SymbolicState (UpdateFunction a) (Maybe Output)

transition :: forall a. (Tuple3 SymbolicState (EnablingFunction a) (Maybe Input)) -> (Tuple3 SymbolicState (UpdateFunction a) (Maybe Output)) -> Transition a
transition (Tuple oldState (Tuple enabling (Tuple inp _))) (Tuple newState (Tuple update (Tuple outp _))) = Transition oldState enabling inp newState update outp

infix 5 transition as ==>

type Transitions a = Array (Transition a)

type AdmissibilityFunction a = SymbolicState -> Variables a -> Boolean

data EFSM a = EFSM (Transitions a) (AdmissibilityFunction a)

stateSet :: forall a. EFSM a -> Set SymbolicState
stateSet (EFSM transitions _) = fromFoldable $ concatMap (\(Transition oldS _ _ newS _ _) -> [oldS, newS]) transitions

inputSet :: forall a. EFSM a -> Set Input
inputSet (EFSM transitions _) = fromFoldable $ mapMaybe (\(Transition _ _ i _ _ _) -> i) transitions

outputSet :: forall a. EFSM a -> Set Output
outputSet (EFSM transitions _) = fromFoldable $ mapMaybe (\(Transition _ _ _ _ _ o) -> o) transitions

enablingFunctionSet :: forall a. EFSM a -> Array (EnablingFunction a)
enablingFunctionSet (EFSM transitions _) = map (\(Transition _ f _ _ _ _) -> f) transitions

updateFunctionSet :: forall a. EFSM a -> Array (UpdateFunction a)
updateFunctionSet (EFSM transitions _) = map (\(Transition _ _ _ _ u _) -> u) transitions

type EFSMConfig a = Tuple SymbolicState (Variables a)

updateVariables :: forall a. EFSM a -> Variables a -> State (EFSMConfig a) Boolean
updateVariables (EFSM _ adm) newAss = do
  Tuple ss _ <- get
  let admitted = adm ss newAss
  if admitted then do
    put (Tuple ss newAss)
    pure true
  else pure false

processInput :: forall a. EFSM a -> Input -> State (EFSMConfig a) (Maybe Output)
processInput (EFSM trs _) inp = do
  Tuple ss as <- get
  let t = findTransition as trs inp ss
  case t of
    Just (Transition _ _ _ snew u op) -> do
      put (Tuple snew (u as))
      pure op
    Nothing -> pure Nothing

findTransition :: forall a. Variables a -> Transitions a -> Input -> SymbolicState -> Maybe (Transition a)
findTransition as trans inp sold = find (\(Transition state f inp' _ _ _) -> sold == state && (Just inp) == inp' && (f as)) trans