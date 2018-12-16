{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day16.Solution
  ( run
  , Registers, Register
  , Program, Instruction(..)
  , OpCode(..)
  , runProgram, executeInstruction
  , runMyProgram, myTranslate
  ) where

import           Data.Bits (Bits(..))
import           Data.Char (isDigit)
import           Data.List (nub, (\\), foldl')
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Text.Parsec


-- | the register of the program is a map of register-indizes to values
-- parametrized over the kind of values stored in the register
type Registers v     = Map Register v

-- | the index into the register
type Register        = Int

-- | Constraint-Kind for Register-Values - Read for parsing the rest for execution
type RegisterValue v = (Integral v, Bits v, Num v, Ord v, Read v)

-- | a program is a list of instructions
type Program opCode regV = [Instruction regV opCode]

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


-- | input of todays problem
-- example pairs of instructions and their effects
-- and a small example program
data Input regV = Input
  { examples :: [Example regV]
  , program  :: Program OpCodeNumber regV
  } deriving Show


-- | an example Pair - the values of the regiserts before, an instruction and the registers after
data Example regV = Example
  { regsBefore  :: Registers regV
  , instruction :: Instruction regV OpCodeNumber
  , regsAfter   :: Registers regV
  } deriving Show


-- | type for the Opcode-Numbers - Int is fine here
type OpCodeNumber  = Int

-- | for part 2 our task is to find the correct Correspondence
-- between 'OpCodeNumber's and 'OpCode's
type Correspondence = Map OpCodeNumber OpCode


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 16"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


----------------------------------------------------------------------
-- part 1:

-- | just find all examples that match at least 3 op-codes
-- and return their count
part1 :: Input Int -> Int
part1 = length . filter ((>= 3) . matchCount) . examples


----------------------------------------------------------------------
-- part 2

-- | find the correspoding op-codes based on the examples
-- and run the program with Int-register-values
part2 :: Input Int -> Int
part2 inp =
  flip getRegister 0 . runProgram . translate corrs $ program inp
  where corrs = genCorrespondence inp


-- | translates a program given with 'OpCodeNumber's into one given 'OpCode's
-- the later can then be executed
translate :: forall regV . Correspondence -> Program OpCodeNumber regV -> Program OpCode regV
translate corr = map translate1
  where
    translate1 :: Instruction regV OpCodeNumber -> Instruction regV OpCode
    translate1 = fmap ((corr !))


-- | iteratively generates a complete Correspondence table
-- by adding 'singleMatches', remove those from consideration
-- and recursively continue
genCorrespondence :: RegisterValue regV => Input regV -> Correspondence
genCorrespondence inp = go Map.empty opCodes
  where
    go corrs opCs =
      let newFound = singleMatches inp corrs opCs
          newCorrs = Map.union corrs $ Map.fromList newFound
          newOpCs  = opCs \\ map snd newFound
      in if null newFound then corrs else go newCorrs newOpCs


-- | finds corresponding Numbers<->OpCodes
-- we surely found one if only one op-code matches all examples for that number
singleMatches :: RegisterValue regV => Input regV -> Correspondence -> [OpCode] -> [(OpCodeNumber, OpCode)]
singleMatches inp corrs opCs =
  nub . map head . filter ((== 1) . length) . map (matches corrs opCs) $ examples inp


-- | looks for matching examples that are not mapped yet
matches :: RegisterValue regV => Correspondence -> [OpCode] -> Example regV -> [(OpCodeNumber, OpCode)]
matches corrs opCs toTest =
  if Map.member opNr corrs
  then []
  else [ (opNr, opC) | opC <- opCs , testOpCode opC toTest ]
  where opNr = opCode $ instruction toTest


----------------------------------------------------------------------
-- find matches in examples

-- | find all matching OpCodes for a given Example
matchCount :: RegisterValue regV => Example regV -> Int
matchCount toTest =
  length $ filter (flip testOpCode toTest) opCodes


-- | replaces an actual op-code for the number and
-- checks if execution of this instruction with the examples input
-- yield the examples output
testOpCode :: RegisterValue regV => OpCode -> Example regV -> Bool
testOpCode opC (Example regsBef prgLine regsAft) =
  let ins = const opC <$> prgLine
      outs = executeInstruction ins regsBef
  in outs == regsAft


----------------------------------------------------------------------
-- run the machine

-- | runs a program by 'executeInstruction' each line
-- begining with empty registers
runProgram :: RegisterValue regV => Program OpCode regV -> Registers regV
runProgram = foldl' (flip executeInstruction) nullRegs
  where nullRegs = Map.fromList $ zip [0..] $ replicate 4 0


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

inputTxt :: RegisterValue regV => IO (Input regV)
inputTxt = parseInput <$> readFile "./src/Day16/input.txt"

----------------------------------------------------------------------
-- parsing

parseInput :: RegisterValue regV => String -> Input regV
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: RegisterValue regV => Parser (Input regV)
inputP = Input <$> ((many1 exampleP) <* newline <* newline) <*> programP


programP :: RegisterValue regV => Parser (Program OpCodeNumber regV)
programP = many1 instructionP


exampleP :: RegisterValue regV => Parser (Example regV)
exampleP =
  Example
  <$> (string "Before: " *> registersInputP <* newline)
  <*> instructionP <* spaces
  <*> (string "After:" *> spaces *> registersInputP <* newline <* newline)


registersInputP :: RegisterValue regV => Parser (Registers regV)
registersInputP =
  Map.fromList . zip [0..] <$> wordsListP
  where
    wordsListP = between (char '[') (char ']') wordsP
    wordsP = numP `sepBy` (char ',' <* spaces)


instructionP :: RegisterValue regV => Parser (Instruction regV OpCodeNumber)
instructionP = Instruction <$> (numP <* space) <*> (numP <* space) <*> (numP <* space) <*> (numP <* space)


numP :: (Read a, Num a) => Parser a
numP = read <$> many1 (satisfy isDigit)

----------------------------------------------------------------------
-- my correspondence (maybe need for other day)

runMyProgram :: RegisterValue regV => Program OpCodeNumber regV -> Registers regV
runMyProgram = runProgram . myTranslate


myTranslate :: Program OpCodeNumber regV -> Program OpCode regV
myTranslate = translate myCorrespondence


myCorrespondence :: Correspondence
myCorrespondence = Map.fromList
  [ (0,BAnR), (1,AddR), (2,EqRI), (3,SetR), (4,GtRR), (5,BOrI)
  , (6,GtIR), (7,SetI), (8,BOrR), (9,BAnI), (10,EqIR), (11,EqRR)
  , (12,GtRI), (13,AddI), (14,MulI), (15,MulR)
  ]
