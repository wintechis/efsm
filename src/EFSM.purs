module EFSM where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Array (concatMap, find)
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, (/\))
import Effect (Effect)
import Effect.Console (logShow)

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

s0 :: SymbolicState
s0 = SymbolicState "s0"
s1 :: SymbolicState
s1 = SymbolicState "s1"
s2 :: SymbolicState
s2 = SymbolicState "s2"
s3 :: SymbolicState 
s3 = SymbolicState "s3"
s4 :: SymbolicState
s4 = SymbolicState "s4"
s5 :: SymbolicState
s5 = SymbolicState "s5"

a :: Input
a = Input "A"
b :: Input
b = Input "B"
c :: Input
c = Input "C"
d :: Input
d = Input "D"

p :: Output
p = Output "P"

type Variables a = Record a
type MyVar = ( pos :: Int, closed :: Boolean, item :: Boolean )

bla :: Variables MyVar
bla = { pos: 1, closed: false, item: false }

newtype Pos = Pos Int
instance showPos :: Show Pos where
  show (Pos pos) = show pos

newtype Closed = Closed Boolean
instance showClosed :: Show Closed where
  show (Closed closed) = show closed

newtype Item = Item Boolean
instance showItem :: Show Item where
  show (Item item) = show item

data Assignment = Assignment Pos Closed Item
instance showAssignment :: Show Assignment where
  show (Assignment pos closed item) = "(" <> show pos <> ", " <> show closed <> ", " <> show item <> ")"

ass :: Int -> Boolean -> Boolean -> Assignment
ass pos closed item = Assignment (Pos pos) (Closed closed) (Item item)

type EnablingFunction a = Variables a -> Boolean

type UpdateFunction a = Variables a -> Variables a

f1 :: EnablingFunction MyVar
f1 _ = true

f2 :: EnablingFunction MyVar
f2 { pos: _, closed, item: _ } = closed

f3 :: EnablingFunction MyVar
f3 { pos: _, closed, item: _ } = not closed

u1 :: UpdateFunction MyVar
u1 { pos: _, closed, item } = { pos: 1, closed: closed, item: item }

u2 :: UpdateFunction MyVar
u2 { pos: _, closed, item } = { pos: 2, closed: closed, item: item }

u3 :: UpdateFunction MyVar
u3 { pos: _, closed, item } = { pos: 3, closed: closed, item: item }

u4 :: UpdateFunction MyVar
u4 { pos, closed, item: _ } = { pos: pos, closed: closed, item: true }

u5 :: UpdateFunction MyVar
u5 { pos, closed, item: _ } = { pos: pos, closed: closed, item: false }

u6 :: UpdateFunction MyVar
u6 { pos: _, closed, item: _ } = { pos: 2, closed: closed, item: false }

data Transition a = Transition SymbolicState (EnablingFunction a) (Maybe Input) SymbolicState (UpdateFunction a) (Maybe Output)

transition :: forall a. (Tuple3 SymbolicState (EnablingFunction a) (Maybe Input)) -> (Tuple3 SymbolicState (UpdateFunction a) (Maybe Output)) -> Transition a
transition (Tuple oldState (Tuple enabling (Tuple inp _))) (Tuple newState (Tuple update (Tuple outp _))) = Transition oldState enabling inp newState update outp

infix 5 transition as ==>

type Transitions a = Array (Transition a)

ts :: Transitions MyVar
ts = [
    s0 /\ f1 /\ Just a /\ unit  ==> s1 /\ u2 /\ Nothing /\ unit,
    s1 /\ f1 /\ Just c /\ unit  ==> s0 /\ u1 /\ Nothing /\ unit,
    s1 /\ f1 /\ Just b /\ unit  ==> s2 /\ u3 /\ Nothing /\ unit,
    s2 /\ f1 /\ Just d /\ unit  ==> s1 /\ u2 /\ Nothing /\ unit,
    s2 /\ f3 /\ Nothing /\ unit ==> s5 /\ u4 /\ Nothing /\ unit,
    s3 /\ f3 /\ Nothing /\ unit ==> s0 /\ u5 /\ Nothing /\ unit,
    s3 /\ f1 /\ Just a /\ unit  ==> s4 /\ u2 /\ Nothing /\ unit,
    s4 /\ f1 /\ Just c /\ unit  ==> s3 /\ u1 /\ Nothing /\ unit,
    s4 /\ f1 /\ Just b /\ unit  ==> s5 /\ u3 /\ Nothing /\ unit,
    s5 /\ f2 /\ Just d /\ unit  ==> s4 /\ u2 /\ Nothing /\ unit,
    s5 /\ f3 /\ Just d /\ unit  ==> s1 /\ u6 /\ Nothing /\ unit
  ]

type AdmissibilityFunction a = SymbolicState -> Variables a -> Boolean

a1 :: AdmissibilityFunction MyVar
a1 (SymbolicState "s0") { pos: 1, closed: _, item: false } = true
a1 (SymbolicState "s1") { pos: 2, closed: _, item: false } = true
a1 (SymbolicState "s2") { pos: 3, closed: _, item: false } = true
a1 (SymbolicState "s3") { pos: 1, closed: _, item: true } = true
a1 (SymbolicState "s4") { pos: 2, closed: _, item: true } = true
a1 (SymbolicState "s5") { pos: 3, closed: _, item: true } = true
a1 _ _ = false

data EFSM a = EFSM (Transitions a) (AdmissibilityFunction a)
efsm :: EFSM MyVar
efsm = EFSM ts a1

stateSet :: forall a. EFSM a -> Set SymbolicState
stateSet (EFSM transitions _) = fromFoldable $ concatMap (\(Transition oldS _ _ newS _ _) -> [oldS, newS]) transitions

type EFSMState a = Tuple SymbolicState (Variables a)

updateVariables :: forall a. EFSM a -> Variables a -> State (EFSMState a) Boolean
updateVariables (EFSM _ adm) newAss = do
  Tuple ss _ <- get
  let admitted = adm ss newAss
  if admitted then do
    put (Tuple ss newAss)
    pure true
  else pure false

processInput :: forall a. EFSM a -> Input -> State (EFSMState a) (Maybe Output)
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

main :: Effect Unit
main = do
  logShow $ stateSet efsm
  logShow $ runState (updateVariables efsm { pos: 1, closed: true, item: false }) (Tuple s0 { pos: 1, closed: false, item: false })
  logShow $ runState (updateVariables efsm { pos: 2, closed: true, item: false }) (Tuple s0 { pos: 1, closed: false, item: false })
  logShow $ runState (processInput efsm a) (Tuple s0 { pos: 1, closed: false, item: false})
  logShow $ runState (processInput efsm b) (Tuple s0 { pos: 1, closed: false, item: false})