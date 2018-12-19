{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
module Day19.Solution where

import           Data.Bits (Bits(..))
import           Data.Char (isDigit)
import           Data.List (foldl')
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Text.Parsec hiding (State)


data State = State
  { ipReg      :: Int
  , program    :: Program OpCode Int
  , insPointer :: Int
  , registers  :: Registers Int
  } deriving Show


-- | the register of the program is a map of register-indizes to values
-- parametrized over the kind of values stored in the register
type Registers v     = Map Register v


-- | the index into the register
type Register        = Int


-- | Constraint-Kind for Register-Values - Read for parsing the rest for execution
type RegisterValue v = (Integral v, Bits v, Num v, Ord v, Read v)


-- | a program is a list of instructions
type Program opCode regV = Map Int  (Instruction regV opCode)


-- | a instruction is parametrized on:
--       - the type of the register-values (the type our calculations are done with - Int seems ok for this proble)
--       - the type of the op-code
-- we want the functor-instance to be on 'opCode' for easy translation
-- between Instructions baes on OpCodeNumber and on OpCodes
data Instruction regV opCode = Instruction
  { opCode   :: opCode       -- ^ the OpCode of the Instruction - this will decide what operation to execute
  , operand1 :: regV         -- ^ the value of the first operand - a register number or a literal number, based on the 'opCode'
  , operand2 :: regV         -- ^ the value of the second operand
  , output   :: Register     -- ^ the number of the register where the outcome of the computation should be written to
  } deriving (Show, Functor)


-- | all possible op-codes - see [problem description](https://adventofcode.com/2018/day/16)
data OpCode
  = AddR | AddI | MulR | MulI
  | BAnR | BAnI | BOrR | BOrI
  | SetR | SetI
  | GtIR | GtRI | GtRR
  | EqIR | EqRI | EqRR
  deriving (Show, Bounded, Enum, Eq, Ord)


run :: IO ()
run = do
  putStrLn "DAY 19"
  prg <- inputTxt

  let end = runProgram prg

  putStrLn $ "part 1: " ++ show ((getRegister $ registers end) 0)


----------------------------------------------------------------------
-- run the machine

-- | runs a program by 'executeInstruction' each line
-- begining with empty registers
runProgram :: State -> State
runProgram state =
  case step state of
    Nothing -> state
    Just state' -> runProgram state'


step :: State -> Maybe State
step state = do
  let regs' = setRegister (ipReg state) (insPointer state) (registers state)
  ins <- getInstruction state (insPointer state)
  let regs'' = executeInstruction ins regs'
  let ip' = getRegister regs'' (ipReg state) + 1
  pure $ state { insPointer = ip', registers = regs'' }


getInstruction :: State -> Int -> Maybe (Instruction Int OpCode)
getInstruction state ip' = Map.lookup ip' (program state)


-- | executes a instruction based on the [rules found here](https://adventofcode.com/2018/day/16)
executeInstruction :: RegisterValue regV => Instruction regV OpCode -> Registers regV -> Registers regV
executeInstruction (Instruction opC op1 op2 toReg) regs =
  let newValue =
        case opC of
          AddR -> get op1 + get op2
          AddI -> get op1 + op2
          MulR -> get op1 * get op2
          MulI -> get op1 * op2
          BAnR -> get op1 .&. get op2
          BAnI -> get op1 .&. op2
          BOrR -> get op1 .|. get op2
          BOrI -> get op1 .|. op2
          SetR -> get op1
          SetI -> op1
          GtIR -> if op1 > get op2 then 1 else 0
          GtRI -> if get op1 > op2 then 1 else 0
          GtRR -> if get op1 > get op2 then 1 else 0
          EqIR -> if op1 == get op2 then 1 else 0
          EqRI -> if get op1 == op2 then 1 else 0
          EqRR -> if get op1 == get op2 then 1 else 0
  in setRegister toReg newValue regs
  where get = getRegister regs . fromIntegral



----------------------------------------------------------------------
-- helpers

opCodes :: [OpCode]
opCodes = [minBound .. maxBound]


getRegister :: Registers regV -> Register -> regV
getRegister regs r = regs ! r


setRegister :: Register -> regV -> Registers regV -> Registers regV
setRegister r v = Map.insert r v

----------------------------------------------------------------------
-- IO

inputTxt :: IO State
inputTxt = parseInput <$> readFile "./src/Day19/input.txt"


inputTst :: IO State
inputTst = parseInput <$> readFile "./src/Day19/test.txt"
----------------------------------------------------------------------
-- parsing

parseInput :: String -> State
parseInput = either (error . show) id . parse stateP "input.txt"


type Parser a = Parsec String () a


stateP :: Parser State
stateP = do
  ipR <- ipRegP <* newline
  prg <- programP
  pure $ State ipR prg 0 (Map.fromList $ zip [0..] $ replicate 6 0)


programP :: RegisterValue regV => Parser (Program OpCode regV)
programP = Map.fromList . zip [0..] <$> many1 instructionP


instructionP :: RegisterValue regV => Parser (Instruction regV OpCode)
instructionP = Instruction <$> (opCodeP <* spaces) <*> (numP <* spaces) <*> (numP <* spaces) <*> (numP <* spaces)


opCodeP :: Parser OpCode
opCodeP = choice (map try opCPs)
  where
    opCPs = map opCP $
      [ ("addr", AddR)
      , ("addi", AddI)
      , ("mulr", MulR)
      , ("muli", MulI)
      , ("banr", BAnR)
      , ("bani", BAnI)
      , ("borr", BOrR)
      , ("bori", BOrI)
      , ("setr", SetR)
      , ("seti", SetI)
      , ("gtir", GtIR)
      , ("gtri", GtRI)
      , ("gtrr", GtRR)
      , ("eqir", EqIR)
      , ("eqri", EqRI)
      , ("eqrr", EqRR)
      ]
    opCP (str, opC) = string str *> pure opC


ipRegP :: Parser Int
ipRegP = string "#ip " *> numP


numP :: (Read a, Num a) => Parser a
numP = read <$> many1 (satisfy isDigit)
