{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
module Day19.Solution
  ( run
  , Registers, Register
  , Program, Instruction(..)
  , OpCode(..)
  , RegisterValue
  , runProgram, executeInstruction
  , getRegister, setRegister
  , programP
  ) where

import           Data.Char (isDigit)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Day16.Solution hiding (runProgram, run, Program)
import           Debug.Trace (trace)
import           Text.Parsec hiding (State)


-- | State for program execution
data State = State
  { ipReg      :: !Int             -- ^ which register should be used for the instruction-pointer
  , program    :: Program Int      -- ^ the program to run
  , insPointer :: !Int             -- ^ the current instruction pointer
  , registers  :: Registers Int    -- ^ the current register values
  } deriving Show


-- | a program is a map of instructions
type Program regV = Map Int (Instruction regV OpCode)


run :: IO ()
run = do
  putStrLn "DAY 19"
  prg <- inputTxt

  let end = runProgram prg
  putStrLn $ "part 1: " ++ show ((getRegister $ registers end) 0)
  putStrLn $ "part 2: " ++ show part2

----------------------------------------------------------------------
-- Part 2

constant :: Int
constant = 10551386


divisors :: Int -> [Int]
divisors n = [ t | t <- [1..n] , n `mod` t == 0 ]


-- | the algorithm is easy: sum up divisors
part2 :: Int
part2 = sum $ divisors constant


-- | used as breakpoits to output registers
-- use this to identify the 'konstante' in your input (it's the lower number staying constant at line 2)
breaks :: [Int]
breaks = []


----------------------------------------------------------------------
-- run the machine

-- | runs a program by 'executeInstruction' each line
-- begining with empty registers
runProgram :: State -> State
runProgram state =
  case step state of
    Nothing -> state
    Just state' -> runProgram state'


-- | one step of operation:
-- sets my ip-register to the ip, executes the current instruction and sets the
-- ip from the ip-register + 1
-- if there is no current instruction quit
step :: State -> Maybe State
step state = do
  ins <- getInstruction state (insPointer state)
  let regsWithIP   = setRegister (ipReg state) (insPointer state) (registers state)
  let regsAfterIns = executeInstruction ins regsWithIP
  let nextIP = getRegister regsAfterIns (ipReg state) + 1
  -- this one is a poor mans breakpoint/trace - just edit 'breaks' and evertime the IP hits one of these values we'll trace all registers
  -- this way you can find out, what your constant actually looks like without hunting for it in the assembler code
  _ <- if nextIP `elem` breaks then (trace (show regsAfterIns)) (Just ()) else Just ()
  pure $ state { insPointer = nextIP, registers = regsAfterIns }


-- | tries to get the current instruction
getInstruction :: State -> Int -> Maybe (Instruction Int OpCode)
getInstruction state ip' = Map.lookup ip' (program state)


----------------------------------------------------------------------
-- IO

inputTxt :: IO State
inputTxt = parseInput <$> readFile "./src/Day19/input.txt"

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


programP :: RegisterValue regV => Parser (Program regV)
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
