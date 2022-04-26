module EFSM where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Array (find)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)

data SymbolicState = SymbolicState String
derive instance eqSymbolicState :: Eq SymbolicState
derive instance ordSymbolicState :: Ord SymbolicState
instance showSymbolicState :: Show SymbolicState where
  show (SymbolicState state) = show state
type SymbolicStates = Set SymbolicState
data Input = Input String
derive instance eqInput :: Eq Input
derive instance ordInput :: Ord Input
type Inputs = Set Input
data Output = Output String
derive instance eqOutput :: Eq Output
derive instance ordOutput :: Ord Output
instance showOutput :: Show Output where
  show (Output output) = show output
type Outputs = Set Output
newtype ValueSpace a = ValueSpace a
type Variable a = Map String (ValueSpace a)
type EnablingFunction a = a -> Boolean
type UpdateFunction a = a -> a

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
s :: SymbolicStates
s = fromFoldable [s0, s1, s2, s3, s4, s5]

a :: Input
a = Input "A"
b :: Input
b = Input "B"
c :: Input
c = Input "C"
d :: Input
d = Input "D"
i :: Inputs
i = fromFoldable [a, b, c, d]

p :: Output
p = Output "P"
o :: Outputs
o = fromFoldable [p]

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

f1 :: Assignment -> Boolean
f1 _ = true

f2 :: Assignment -> Boolean
f2 (Assignment _ (Closed v) _) = v

f3 :: Assignment -> Boolean
f3 (Assignment _ (Closed v) _) = not v

type EnablingFunctions = Array (Assignment -> Boolean)
fs :: EnablingFunctions
fs = [f1, f2, f3]

u1 :: Assignment -> Assignment
u1 (Assignment _ closed item) = Assignment Pos1 closed item

u2 :: Assignment -> Assignment
u2 (Assignment _ closed item) = Assignment Pos2 closed item

u3 :: Assignment -> Assignment
u3 (Assignment _ closed item) = Assignment Pos3 closed item

u4 :: Assignment -> Assignment
u4 (Assignment pos closed _) = Assignment pos closed (Item true)

u5 :: Assignment -> Assignment
u5 (Assignment pos closed _) = Assignment pos closed (Item false)

u6 :: Assignment -> Assignment
u6 (Assignment _ closed _) = Assignment Pos2 closed (Item false)

type UpdateFunctions = Array (Assignment -> Assignment)
us :: UpdateFunctions
us = [u1, u2, u3, u4, u5, u6]

data Transition = Transition SymbolicState (Assignment -> Boolean) (Maybe Input) SymbolicState (Assignment -> Assignment) (Maybe Output)

t1 :: Transition
t1 = Transition s0 f1 (Just a) s1 u2 Nothing

t2 :: Transition
t2 = Transition s1 f1 (Just c) s0 u1 Nothing

t3 :: Transition
t3 = Transition s1 f1 (Just b) s2 u3 Nothing

t4 :: Transition
t4 = Transition s2 f1 (Just d) s1 u2 Nothing

t5 :: Transition
t5 = Transition s2 f3 Nothing s5 u4 Nothing

t6 :: Transition
t6 = Transition s3 f3 Nothing s0 u5 Nothing

t7 :: Transition
t7 = Transition s3 f1 (Just a) s4 u2 Nothing

t8 :: Transition
t8 = Transition s4 f1 (Just c) s3 u1 Nothing

t9 :: Transition
t9 = Transition s4 f1 (Just b) s5 u3 Nothing

t10 :: Transition
t10 = Transition s5 f2 (Just d) s4 u2 Nothing

t11 :: Transition
t11 = Transition s5 f3 (Just d) s1 u6 Nothing

type Transitions = Array Transition
ts :: Transitions
ts = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11]

a1 :: SymbolicState -> Assignment -> Boolean
a1 (SymbolicState "s0") (Assignment Pos1 _ (Item false)) = true
a1 (SymbolicState "s1") (Assignment Pos2 _ (Item false)) = true
a1 (SymbolicState "s2") (Assignment Pos3 _ (Item false)) = true
a1 (SymbolicState "s3") (Assignment Pos1 _ (Item true)) = true
a1 (SymbolicState "s4") (Assignment Pos2 _ (Item true)) = true
a1 (SymbolicState "s5") (Assignment Pos3 _ (Item true)) = true
a1 _ _ = false

type AdmissibilityFunction = SymbolicState -> Assignment -> Boolean

data EFSM = EFSM SymbolicStates Inputs Outputs EnablingFunctions UpdateFunctions Transitions AdmissibilityFunction
e :: EFSM
e = EFSM s i o fs us ts a1

type EFSMState = Tuple SymbolicState Assignment

updateVariables :: EFSM -> Assignment -> State EFSMState Boolean
updateVariables (EFSM _ _ _ _ _ _ adm) newAss = do
  Tuple ss _ <- get
  let admitted = adm ss newAss
  if admitted then do
    put (Tuple ss newAss)
    pure true
  else pure false

input :: EFSM -> Input -> State EFSMState (Maybe Output)
input (EFSM _ _ _ _ _ trs _) inp = do
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
  logShow $ runState (updateVariables e (ass 1 true false)) (Tuple s0 (ass 1 false false))
  logShow $ runState (updateVariables e (ass 2 true false)) (Tuple s0 (ass 1 false false))
  logShow $ runState (input e a) (Tuple s0 (ass 1 false false))
  logShow $ runState (input e b) (Tuple s0 (ass 1 false false))

{-
type EFSMState = State (Tuple SymbolicState Assignment) (Maybe Output)

initState :: EFSMState
initState = put $ Tuple s0 (Assignment Pos1 (Closed false) (Item false))

newState :: SymbolicState -> EFSMState
newState ss = do--modify (\(Tuple _ ass) -> (Tuple ss ass)) Nothing
  Tuple st ass <- gets (\_ -> Nothing)
  put $ Tuple ss ass

asd :: Tuple SymbolicState Assignment
asd = execState (newState s1) $ execState initState $ Tuple s0 (Assignment Pos1 (Closed true) (Item false))

main :: Effect Unit
main = logShow asd


data Run = Run EFSM SymbolicState Assignment
run :: Run
run = Run e s0 (Assignment Pos1 (Closed false) (Item false))

findTransition :: Assignment -> Transitions -> Input -> SymbolicState -> Maybe Transition
findTransition ass trans inp sold = find (\(Transition state f i _ _ _) -> sold == state && (Just inp) == i && (f ass)) trans

applyTransition :: Transition -> Run -> Run
applyTransition (Transition _ _ _ snew u o) (Run e _ ass) = Run e snew $ u ass

input :: Input -> State Run Unit
input inp = modify_ $ (\(Run (EFSM _ _ _ _ _ trs _) sold ass) -> applyTransition <$> findTransition ass trs inp sold)
-}