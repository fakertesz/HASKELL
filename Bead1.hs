{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Bead1 where

import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.String
import Data.Maybe
import Debug.Trace

data ProgState = ProgState {
    r1     :: Int,
    r2     :: Int,
    r3     :: Int,
    cmp    :: Ordering,
    memory :: [Int]
} deriving (Eq, Show)

startState :: ProgState
startState = ProgState 0 0 0 EQ (replicate 10 0)

data Register
  = R1
  | R2
  | R3
  deriving (Eq, Show)

data Destination
  = DstReg Register     -- regiszterbe írunk
  | DstDeref Register   -- memóriába írunk, az adott regiszterben tárolt index helyére
  deriving (Eq, Show)

data Source
  = SrcReg Register     -- regiszterből olvasunk
  | SrcDeref Register   -- memóriából olvasunk, az adott regiszterben tárolt index helyéről
  | SrcLit Int          -- szám literál
  deriving (Eq, Show)

data Instruction
  = Mov Destination Source   -- írjuk a Destination-be a Source értékét
  | Add Destination Source   -- adjuk a Destination-höz a Source értékét
  | Mul Destination Source   -- szorozzuk a Destination-t a Source értékével
  | Sub Destination Source   -- vonjuk ki a Destination-ből a Source értékét
  | Cmp Source Source        -- hasonlítsunk össze két Source értéket `compare`-el, az eredményt
                             -- írjuk a `cmp` regiszterbe

  | Jeq Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `EQ` van
  | Jlt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `LT` van
  | Jgt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `GT` van
  deriving (Eq, Show)

type Label = String  -- címke a programban, ahová ugrani lehet


-- Beírunk r1-be 10-et, r2-be 20-at
p1 :: RawProgram
p1 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Left "l1",                            -- tehetünk bárhova címkét, nem muszáj használni a programban
  Right $ Mov (DstReg R2) (SrcLit 20)
  ]

-- Kiszámoljuk 10 faktoriálisát, az eredményt r2-ben tároljuk
p2 :: RawProgram
p2 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Right $ Mov (DstReg R2) (SrcLit 1),
  Left "loop",
  Right $ Mul (DstReg R2) (SrcReg R1),
  Right $ Sub (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 0),
  Right $ Jgt "loop"
  ]

-- Feltöltjük 0-9-el a memóriát
p3 :: RawProgram
p3 = [
  Left "start",
  Right $ Mov (DstDeref R1) (SrcReg R1),
  Right $ Add (DstReg R1) (SrcLit 1),
  Right $ Cmp (SrcReg R1) (SrcLit 10),
  Right $ Jlt "start"
  ]

-- Megnöveljük 1-el a memória összes mezőjét
p4 :: RawProgram
p4 = [
  Left "start",
  Right $ Add (DstDeref R2) (SrcLit 1),
  Right $ Add (DstReg R2) (SrcLit 1),
  Right $ Cmp (SrcReg R2) (SrcLit 10),
  Right $ Jlt "start"
  ]

-- Kétszer hozzáadunk 1-et a harmadik regiszterhez
p5 :: RawProgram
p5 = [
  Left "start",
  Right $ Jeq "first",
  Left "first",
  Right $ Add (DstReg R3) (SrcLit 1),
  Left "second",
  Right $ Add (DstReg R3) (SrcLit 1)
  ]

type RawProgram = [Either Label Instruction]
type Program = [(Label, [Instruction])]

{- 1. feladat -}

mapProgram :: Instruction -> Program -> Program
mapProgram inst = map (\(l, i) -> (l, i ++ [inst]))

foldProgram :: RawProgram -> Program -> Program
foldProgram [] acc = acc
foldProgram ((Left x):xs) acc = foldProgram xs (acc ++ [(x, [])])
foldProgram ((Right x):xs) acc = foldProgram xs (mapProgram x acc)

toProgram :: RawProgram -> Program
toProgram x = foldProgram x []

testToProgram :: Bool
testToProgram = toProgram p1 == [("start",[Mov (DstReg R1) (SrcLit 10),Mov (DstReg R2) (SrcLit 20)]),("l1",[Mov (DstReg R2) (SrcLit 20)])]
                &&
                toProgram [Left "start", Left "l1", Right $ Mov (DstReg R1) (SrcLit 10)] == [("start",[Mov (DstReg R1) (SrcLit 10)]),("l1",[Mov (DstReg R1) (SrcLit 10)])]
                &&
                toProgram [Left "start", Right $ Mov (DstReg R1) (SrcLit 10),  Left "l1"] == [("start",[Mov (DstReg R1) (SrcLit 10)]),("l1",[])]


-- Írjunk kiértékelőt! Használjuk a State monádot!

type M a = State ProgState a

{- 2. feladat -}

updateMemory :: [Int] -> State ProgState ()
updateMemory x =
  modify
  (\state' -> state' { memory = x })

overrideRegisterState :: Register -> Int -> State ProgState ()
overrideRegisterState x y =
  modify
  (\state -> case x of
    R1 -> state { r1 = y }
    R2 -> state { r2 = y }
    R3 -> state { r3 = y }
  )

addToRegisterValue :: Register -> Int -> State ProgState ()
addToRegisterValue x y =
  modify
  (\state -> case x of
    R1 -> state { r1 = r1 state + y }
    R2 -> state { r2 = r2 state + y }
    R3 -> state { r3 = r3 state + y }
  )
subtractFromRegisterValue :: Register -> Int -> State ProgState ()
subtractFromRegisterValue x y =
  modify
  (\state -> case x of
    R1 -> state { r1 = r1 state - y }
    R2 -> state { r2 = r2 state - y }
    R3 -> state { r3 = r3 state - y }
  )

multiplyRegisterValue :: Register -> Int -> State ProgState ()
multiplyRegisterValue x y =
  modify
  (\state -> case x of
    R1 -> state { r1 = r1 state * y}
    R2 -> state { r2 = r2 state * y}
    R3 -> state { r3 = r3 state * y}
  )

writeToCmp :: Int -> Int -> State ProgState()
writeToCmp x y =
  modify
  (\state' -> state' { cmp = compare x y })

getRegisterState :: Register -> ProgState -> Int
getRegisterState R1 state = r1 state
getRegisterState R2 state = r2 state
getRegisterState R3 state = r3 state

jmp :: Label -> Program -> [Instruction]
jmp l p = case lookup l p of
    Just instructions -> instructions
    _ -> []

eval :: Program -> [Instruction] -> M ()
eval [] _ = return ()
eval _ [] = return ()

eval p ((Mov (DstReg register) (SrcLit value)):xs) =
  do
    overrideRegisterState register value
    eval p xs

eval p ((Add (DstReg register) (SrcLit value)):xs) =
  do
    addToRegisterValue register value
    eval p xs

eval p ((Sub (DstReg register) (SrcLit value)):xs) =
  do
    subtractFromRegisterValue register value
    eval p xs

eval p ((Mul (DstReg dest) (SrcReg src)):xs) =
  do
    state <- get
    let multiplier = getRegisterState src state
    multiplyRegisterValue dest multiplier
    eval p xs

-- register érték csere
eval p ((Mov (DstDeref dest) (SrcReg src)):xs) =
  do
    state <- get
    let valueOne = getRegisterState dest state
    let valueTwo = getRegisterState src state
    let asd = take valueOne (memory state) ++ [valueTwo] ++ drop (valueOne + 1) (memory state)
    updateMemory asd
    eval p xs

eval p ((Add (DstDeref register) (SrcLit value)):xs) =
  do
    state <- get
    let val = getRegisterState register state
    let memVal = memory state !! val
    updateMemory (take val (memory state) ++ [memVal + value] ++ drop (val + 1) (memory state))
    eval p xs

eval p (Cmp (SrcReg register) (SrcLit value):xs) =
  do
    state <- get
    let val = getRegisterState register state
    modify (\state' -> state' { cmp = compare val value })
    eval p xs

eval p (x:xs) = case x of
  Jeq l -> do
    state <- get
    if cmp state == EQ
      then eval p (jmp l p)
      else eval p xs
  Jgt l -> do
    state <- get
    if cmp state == GT
      then eval p (jmp l p)
      else eval p xs
  Jlt l -> do
    state <- get
    if cmp state == LT
      then eval p (jmp l p)
      else eval p xs

  _ -> eval p xs


-- futtatunk egy nyers programot a startState-ből kiindulva
runProgram :: RawProgram -> ProgState
runProgram rprog = case toProgram rprog of
  []                  -> startState
  prog@((_, start):_) -> execState (eval prog start) startState

testRunProgram :: Bool
testRunProgram = runProgram p1 == ProgState {r1 = 10, r2 = 20, r3 = 0, cmp = EQ, memory = [0,0,0,0,0,0,0,0,0,0]}
                && runProgram p2 == ProgState {r1 = 0, r2 = 3628800, r3 = 0, cmp = EQ, memory = [0,0,0,0,0,0,0,0,0,0]}
                && runProgram p3 == ProgState {r1 = 10, r2 = 0, r3 = 0, cmp = EQ, memory = [0,1,2,3,4,5,6,7,8,9]}
                && runProgram p4 == ProgState {r1 = 0, r2 = 10, r3 = 0, cmp = EQ, memory = [1,1,1,1,1,1,1,1,1,1]}
                && runProgram p5 == ProgState {r1 = 0, r2 = 0, r3 = 2, cmp = EQ, memory = [0,0,0,0,0,0,0,0,0,0]}