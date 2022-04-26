module EFSM where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Array (find)
import Data.Maybe (Maybe(..))
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

data Pos = Pos1 | Pos2 | Pos3
instance showPos :: Show Pos where
  show Pos1 = "1"
  show Pos2 = "2"
  show Pos3 = "3"

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
ass 1 closed item = Assignment Pos1 (Closed closed) (Item item)
ass 2 closed item = Assignment Pos2 (Closed closed) (Item item)
ass 3 closed item = Assignment Pos3 (Closed closed) (Item item)
ass _ closed item = Assignment Pos1 (Closed closed) (Item item)

type EnablingFunction = Assignment -> Boolean

type UpdateFunction = Assignment -> Assignment

f1 :: EnablingFunction
f1 _ = true

f2 :: EnablingFunction
f2 (Assignment _ (Closed v) _) = v

f3 :: EnablingFunction
f3 (Assignment _ (Closed v) _) = not v

u1 :: UpdateFunction
u1 (Assignment _ closed item) = Assignment Pos1 closed item

u2 :: UpdateFunction
u2 (Assignment _ closed item) = Assignment Pos2 closed item

u3 :: UpdateFunction
u3 (Assignment _ closed item) = Assignment Pos3 closed item

u4 :: UpdateFunction
u4 (Assignment pos closed _) = Assignment pos closed (Item true)

u5 :: UpdateFunction
u5 (Assignment pos closed _) = Assignment pos closed (Item false)

u6 :: UpdateFunction
u6 (Assignment _ closed _) = Assignment Pos2 closed (Item false)

data Transition = Transition SymbolicState EnablingFunction (Maybe Input) SymbolicState (Assignment -> Assignment) (Maybe Output)

transition :: (Tuple3 SymbolicState EnablingFunction (Maybe Input)) -> (Tuple3 SymbolicState UpdateFunction (Maybe Output)) -> Transition
transition (Tuple oldState (Tuple enabling (Tuple inp _))) (Tuple newState (Tuple update (Tuple outp _))) = Transition oldState enabling inp newState update outp

infix 5 transition as ==>

type Transitions = Array Transition

ts :: Transitions
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

type AdmissibilityFunction = SymbolicState -> Assignment -> Boolean

a1 :: AdmissibilityFunction
a1 (SymbolicState "s0") (Assignment Pos1 _ (Item false)) = true
a1 (SymbolicState "s1") (Assignment Pos2 _ (Item false)) = true
a1 (SymbolicState "s2") (Assignment Pos3 _ (Item false)) = true
a1 (SymbolicState "s3") (Assignment Pos1 _ (Item true)) = true
a1 (SymbolicState "s4") (Assignment Pos2 _ (Item true)) = true
a1 (SymbolicState "s5") (Assignment Pos3 _ (Item true)) = true
a1 _ _ = false

data EFSM = EFSM Transitions AdmissibilityFunction
efsm :: EFSM
efsm = EFSM ts a1

type EFSMState = Tuple SymbolicState Assignment

updateVariables :: EFSM -> Assignment -> State EFSMState Boolean
updateVariables (EFSM _ adm) newAss = do
  Tuple ss _ <- get
  let admitted = adm ss newAss
  if admitted then do
    put (Tuple ss newAss)
    pure true
  else pure false

processInput :: EFSM -> Input -> State EFSMState (Maybe Output)
processInput (EFSM trs _) inp = do
  Tuple ss as <- get
  let t = findTransition as trs inp ss
  case t of
    Just (Transition _ _ _ snew u op) -> do
      put (Tuple snew (u as))
      pure op
    Nothing -> pure Nothing

findTransition :: Assignment -> Transitions -> Input -> SymbolicState -> Maybe Transition
findTransition as trans inp sold = find (\(Transition state f inp' _ _ _) -> sold == state && (Just inp) == inp' && (f as)) trans

main :: Effect Unit
main = do
  logShow $ runState (updateVariables efsm (ass 1 true false)) (Tuple s0 (ass 1 false false))
  logShow $ runState (updateVariables efsm (ass 2 true false)) (Tuple s0 (ass 1 false false))
  logShow $ runState (processInput efsm a) (Tuple s0 (ass 1 false false))
  logShow $ runState (processInput efsm b) (Tuple s0 (ass 1 false false))