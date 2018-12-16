{-# LANGUAGE DeriveFunctor #-}
module Day16.Solution
  ( run
  , Registers, Register, RegisterValue
  , Program, Instruction(..), OpCodeValue
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


-- | all calculations should happen based on this type
type Value         = Int


-- | the register of the program is a map of register-indizes to values
type Registers     = Map Register RegisterValue
type Register      = Int
type RegisterValue = Value


-- | a program is a list of instructions
type Program opCode = [Instruction opCode]

-- | a instruction is parametrized on the type of the op-code
-- (an here be either an number or an actual op-code)
data Instruction opCode = Instruction
  { opCode   :: opCode       -- ^ the OpCode of the Instruction - this will decide what operation to execute
  , operand1 :: OpCodeValue  -- ^ the value of the first operand - a register number or a literal number, based on the 'opCode'
  , operand2 :: OpCodeValue  -- ^ the value of the second operand
  , output   :: Register     -- ^ the number of the register where the outcome of the computation should be written to
  } deriving (Show, Functor)

type OpCodeValue = Value

-- | all possible op-codes - see [problem description](https://adventofcode.com/2018/day/16)
data OpCode
  = AddR
  | AddI
  | MulR
  | MulI
  | BAnR
  | BAnI
  | BOrR
  | BOrI
  | SetR
  | SetI
  | GtIR
  | GtRI
  | GtRR
  | EqIR
  | EqRI
  | EqRR
  deriving (Show, Bounded, Enum, Eq, Ord)


-- | input of todays problem
-- example pairs of instructions and their effects
-- and a small example program
data Input = Input
  { examples :: [Example]
  , program  :: Program OpCodeNumber
  } deriving Show


-- | an example Pair - the values of the regiserts before, an instruction and the registers after
data Example = Example
  { regsBefore  :: Registers
  , instruction :: Instruction OpCodeNumber
  , regsAfter   :: Registers
  } deriving Show


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
part1 :: Input -> Int
part1 = length . filter ((>= 3) . matchCount) . examples


----------------------------------------------------------------------
-- part 2

part2 :: Input -> RegisterValue
part2 inp =
  flip getRegister 0 . runProgram . translate corrs $ program inp
  where corrs = genCorrespondence inp


-- | translates a program given with 'OpCodeNumber's into one given 'OpCode's
-- the later can then be executed
translate :: Correspondence -> Program OpCodeNumber -> Program OpCode
translate corr = map translate1
  where translate1 = fmap (corr !)


-- | iteratively generates a complete Correspondence table
-- by adding 'singleMatches', remove those from consideration
-- and recursively continue
genCorrespondence :: Input -> Correspondence
genCorrespondence inp = go Map.empty opCodes
  where
    go corrs opCs =
      let newFound = singleMatches inp corrs opCs
          newCorrs = Map.union corrs $ Map.fromList newFound
          newOpCs  = opCs \\ map snd newFound
      in if null newFound then corrs else go newCorrs newOpCs


-- | finds corresponding Numbers<->OpCodes
-- we surely found one if only one op-code matches all examples for that number
singleMatches :: Input -> Correspondence -> [OpCode] -> [(OpCodeNumber, OpCode)]
singleMatches inp corrs opCs =
  nub . map head . filter ((== 1) . length) . map (matches corrs opCs) $ examples inp


-- | looks for matching examples that are not mapped yet
matches :: Correspondence -> [OpCode] -> Example -> [(OpCodeNumber, OpCode)]
matches corrs opCs toTest =
  if Map.member opNr corrs
  then []
  else [ (opNr, opC) | opC <- opCs , testOpCode opC toTest ]
  where opNr = opCode $ instruction toTest


----------------------------------------------------------------------
-- find matches in examples

-- | find all matching OpCodes for a given Example
matchCount :: Example -> Int
matchCount toTest =
  length $ filter (flip testOpCode toTest) opCodes


-- | replaces an actual op-code for the number and
-- checks if execution of this instruction with the examples input
-- yield the examples output
testOpCode :: OpCode -> Example -> Bool
testOpCode opC (Example regsBef prgLine regsAft) =
  let ins = const opC <$> prgLine
      outs = executeInstruction ins regsBef
  in outs == regsAft


----------------------------------------------------------------------
-- run the machine

-- | runs a program by 'executeInstruction' each line
-- begining with empty registers
runProgram :: Program OpCode -> Registers
runProgram = foldl' (flip executeInstruction) nullRegs
  where nullRegs = Map.fromList $ zip [0..] $ replicate 4 0


-- | executes a instruction based on the [rules found here](https://adventofcode.com/2018/day/16)
executeInstruction :: Instruction OpCode -> Registers -> Registers
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


getRegister :: Registers -> Register -> RegisterValue
getRegister regs r = regs ! r


setRegister :: Register -> RegisterValue -> Registers -> Registers
setRegister r v = Map.insert r v

----------------------------------------------------------------------
-- IO

inputTxt :: IO Input
inputTxt = parseInput <$> readFile "./src/Day16/input.txt"

----------------------------------------------------------------------
-- parsing

parseInput :: String -> Input
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: Parser Input
inputP = Input <$> ((many1 exampleP) <* newline <* newline) <*> programP


programP :: Parser (Program OpCodeNumber)
programP = many1 instructionP


exampleP :: Parser Example
exampleP =
  Example
  <$> (string "Before: " *> registersInputP <* newline)
  <*> instructionP <* spaces
  <*> (string "After:" *> spaces *> registersInputP <* newline <* newline)


registersInputP :: Parser Registers
registersInputP =
  Map.fromList . zip [0..] <$> wordsListP
  where
    wordsListP = between (char '[') (char ']') wordsP
    wordsP = numP `sepBy` (char ',' <* spaces)


instructionP :: Parser (Instruction OpCodeNumber)
instructionP = Instruction <$> (numP <* space) <*> (numP <* space) <*> (numP <* space) <*> (numP <* space)


numP :: (Read a, Num a) => Parser a
numP = read <$> many1 (satisfy isDigit)

----------------------------------------------------------------------
-- my correspondence (maybe need for other day)

runMyProgram :: Program OpCodeNumber -> Registers
runMyProgram = runProgram . myTranslate


myTranslate :: Program OpCodeNumber -> Program OpCode
myTranslate = translate myCorrespondence


myCorrespondence :: Correspondence
myCorrespondence = Map.fromList
  [ (0,BAnR), (1,AddR), (2,EqRI), (3,SetR), (4,GtRR), (5,BOrI)
  , (6,GtIR), (7,SetI), (8,BOrR), (9,BAnI), (10,EqIR), (11,EqRR)
  , (12,GtRI), (13,AddI), (14,MulI), (15,MulR)
  ]
