module Day16.Solution where

import           Data.Bits (Bits(..))
import           Data.Char (isDigit)
import           Data.List (nub, (\\), foldl')
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Text.Parsec




type Registers = Map Register Int
type Register = Int

type OpCodeNumber = Int


data Value
  = Literal  Int
  | Register Register
  deriving Show


data Instruction = Instruction
  { insOpCode   :: OpCode
  , insOperand1 :: Int
  , insOperand2 :: Int
  , insOutput   :: Register
  } deriving Show


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


data Input = Input
  { examples :: [OpCodePair]
  , program  :: [OpCodeInput]
  } deriving Show


data OpCodePair = OpCodePair
  { regsBefore :: Registers
  , opCode     :: OpCodeInput
  , regsAfter  :: Registers
  } deriving Show


data OpCodeInput = OpCodeInput
  { inpNumber   :: OpCodeNumber
  , inpOperand1 :: Int
  , inpOperand2 :: Int
  , inpOutput   :: Register
  } deriving Show


type Correspondence = Map OpCodeNumber OpCode


run :: IO ()
run = do
  putStrLn "DAY 16"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)

----------------------------------------------------------------------
-- part 1:

part1 :: Input -> Int
part1 = length . filter ((>= 3) . matchCount) . examples


matchCount :: OpCodePair -> Int
matchCount toTest =
  length $ filter (flip testOpCode toTest) opCodes


----------------------------------------------------------------------
-- part 2

part2 :: Input -> Int
part2 inp =
  flip getRegister 0 . runProgram . translate corrs $ program inp
  where corrs = genCorrespondence inp


translate :: Correspondence -> [OpCodeInput] -> [Instruction]
translate corr = map translate1
  where
    translate1 (OpCodeInput opNr op1 op2 out) = Instruction (corr ! opNr) op1 op2 out


genCorrespondence :: Input -> Correspondence
genCorrespondence inp = go Map.empty opCodes
  where
    go corrs opCs =
      let newFound = singleMatches inp corrs opCs
          newCorrs = Map.union corrs $ Map.fromList newFound
          newOpCs  = opCs \\ map snd newFound
      in if null newFound then corrs else go newCorrs newOpCs


singleMatches :: Input -> Correspondence -> [OpCode] -> [(OpCodeNumber, OpCode)]
singleMatches inp corrs opCs =
  nub . map head . filter ((== 1) . length) . map (matches corrs opCs) $ examples inp


matches :: Correspondence -> [OpCode] -> OpCodePair -> [(OpCodeNumber, OpCode)]
matches corrs opCs toTest =
  if Map.member opNr corrs
  then []
  else [ (opNr, opC) | opC <- opCs , testOpCode opC toTest ]
  where opNr = inpNumber $ opCode toTest


testOpCode :: OpCode -> OpCodePair -> Bool
testOpCode opC (OpCodePair regsBef prgLine regsAft) =
  let ins = Instruction opC (inpOperand1 prgLine) (inpOperand2 prgLine) (inpOutput prgLine)
      outs = executeInstruction ins regsBef
  in outs == regsAft


opCodes :: [OpCode]
opCodes = [minBound .. maxBound]


test :: OpCodePair
test = OpCodePair (Map.fromList $ zip [0..] [3,2,1,1]) (OpCodeInput 9 2 1 2) (Map.fromList $ zip [0..] [3,2,2,1])


runProgram :: [Instruction] -> Registers
runProgram = foldl' (flip executeInstruction) nullRegs
  where nullRegs = Map.fromList $ zip [0..] $ replicate 4 0


executeInstruction :: Instruction -> Registers -> Registers
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



getRegister :: Registers -> Register -> Int
getRegister regs r = regs ! r


setRegister :: Register -> Int -> Registers -> Registers
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
inputP = Input <$> ((many1 opCodePairP) <* newline <* newline) <*> programP


programP :: Parser [OpCodeInput]
programP = many1 opCodeInputP


opCodePairP :: Parser OpCodePair
opCodePairP =
  OpCodePair
  <$> (string "Before: " *> registersInputP <* newline)
  <*> opCodeInputP <* spaces
  <*> (string "After:" *> spaces *> registersInputP <* newline <* newline)


registersInputP :: Parser Registers
registersInputP =
  Map.fromList . zip [0..] <$> wordsListP
  where
    wordsListP = between (char '[') (char ']') wordsP
    wordsP = numP `sepBy` (char ',' <* spaces)


opCodeInputP :: Parser OpCodeInput
opCodeInputP = OpCodeInput <$> (numP <* space) <*> (numP <* space) <*> (numP <* space) <*> (numP <* space)


intP :: (Read a, Num a) => Parser a
intP = choice [ negate <$> (char '-' *> numP), numP ]


numP :: (Read a, Num a) => Parser a
numP = read <$> many1 (satisfy isDigit)


