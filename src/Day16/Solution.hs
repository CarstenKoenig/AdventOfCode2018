module Day16.Solution where

import           Data.Bits (Bits(..))
import           Data.Char (isDigit)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word8)
import           Text.Parsec




type Registers = Map Register Word8
type Register = Int

type OpCodeNumber = Int


data Value
  = Literal  Word8
  | Register Register
  deriving Show


data Instruction = Instruction
  { insOpCode   :: OpCode
  , insOperand1 :: Value
  , insOperand2 :: Value
  , insOutput   :: Value
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
  deriving Show


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
  , inpOperand1 :: Word8
  , inpOperand2 :: Word8
  , inpOutput   :: Register
  } deriving Show


run :: IO ()
run = do
  putStrLn "DAY 16"


inputTxt :: IO String
inputTxt = readFile "./src/Day16/input.txt"


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


