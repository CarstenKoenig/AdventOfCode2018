module Day13.Solution where

import           Data.List (foldl', sortBy, group)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (listToMaybe, isNothing, fromJust)
import           Data.Ord (comparing)
import           Data.Tuple (swap)
import           Debug.Trace (traceShowId)
import           Prelude hiding (Left, Right)


data Input = Input
  { carts  :: Carts
  , tracks :: Tracks
  } deriving Show


type Coord = (Int, Int)

type Carts =
  Map Coord Cart


data Cart = Cart
  { cartDir :: CartDir
  , cartBeh :: [Turns]
  }

instance Show Cart where
  show (Cart dir _) = "cart [" ++ show dir ++ "]"


data Turns = GoLeft | GoRight | GoStraight
  deriving Show

type Tracks =
  Map Coord Track


data CartDir
  = Up
  | Down
  | Left
  | Right
  deriving Show


data Track
  = Vertical
  | Horizontal
  | TurnLeft
  | TurnRight
  | Intersection
  deriving Show


run :: IO ()
run = do
  putStrLn "DAY 13"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


part1 :: Input -> Coord
part1 = moveToCollision


part2 :: Input -> Coord
part2 = removeTillLast


removeTillLast :: Input -> Coord
removeTillLast inp = go inp
  where
    go inp' =
      if M.size (carts inp') == 1 then
        head . M.keys $ carts inp'
      else
        go $ foldl' (flip moveCartWithoutCol) inp' $ cartCoords inp'


step :: Input -> Input
step inp =
      if M.size (carts inp) == 1 then
        error (show $ head $ M.keys $ carts inp)
      else
        foldl' (flip moveCartWithoutCol) inp $ cartCoords inp

moveToCollision :: Input -> Coord
moveToCollision inp = go inp
  where
    go inp' =
      case moveCarts inp' (cartCoords inp') of
        Collision c -> c
        Cont inp'' -> go inp''
    moveCarts inp' [] = Cont inp'
    moveCarts inp' (c:cs) =
      case moveCart c inp' of
        Cont inp'' -> moveCarts inp'' cs
        col -> col


cartCoords :: Input -> [Coord]
cartCoords = sortBy (comparing swap) . M.keys . carts


data MoveRes = Cont Input | Collision Coord


moveCart :: Coord -> Input -> MoveRes
moveCart coord@(x,y) inp@(Input cs ts)
  | not isCollision = Cont $ inp { carts = M.insert nextCoord nextCart withOut }
  | otherwise = Collision nextCoord
  where
    withOut = M.delete coord cs
    isCollision = nextCoord `M.member` withOut
    nextCart = Cart nextDir nextBeh
    (nextDir, nextBeh) =
      case (curDir, getTrack ts nextCoord) of
        (Up, TurnLeft) -> (Left, curBeh)
        (Up, TurnRight) -> (Right, curBeh)
        (Up, Vertical) -> (Up, curBeh)
        (Left, TurnLeft) -> (Up, curBeh)
        (Left, TurnRight) -> (Down, curBeh)
        (Left, Horizontal) -> (Left, curBeh)
        (Down, TurnLeft) -> (Right, curBeh)
        (Down, TurnRight) -> (Left, curBeh)
        (Down, Vertical) -> (Down, curBeh)
        (Right, TurnLeft) -> (Down, curBeh)
        (Right, TurnRight) -> (Up, curBeh)
        (Right, Horizontal) -> (Right, curBeh)
        (_ , Intersection) -> nextDesc
        (d,t) -> error $ "invalid combination: " ++ show d ++ " and " ++ show t
    (Cart curDir curBeh) = getCart cs coord
    nextCoord =
      case curDir of
        Left -> (x-1,y)
        Right -> (x+1,y)
        Up -> (x,y-1)
        Down -> (x,y+1)
    nextDesc =
      case (curDir, curBeh) of
        (_, []) -> error "no behavior left"
        (d, GoStraight:rest) -> (d, rest)
        (Up, GoLeft:rest) -> (Left, rest)
        (Up, GoRight:rest) -> (Right, rest)
        (Left, GoLeft:rest) -> (Down, rest)
        (Left, GoRight:rest) -> (Up, rest)
        (Right, GoRight:rest) -> (Down, rest)
        (Right, GoLeft:rest) -> (Up, rest)
        (Down, GoLeft:rest) -> (Right, rest)
        (Down, GoRight:rest) -> (Left, rest)


moveCartWithoutCol :: Coord -> Input -> Input
moveCartWithoutCol coord@(x,y) inp@(Input cs ts)
  | not (cartAt cs coord) = inp
  | not isCollision = inp { carts = M.insert nextCoord nextCart withOut }
  | otherwise = inp { carts = M.delete nextCoord withOut }
  where
    withOut = M.delete coord cs
    isCollision = nextCoord `M.member` withOut
    nextCart = Cart nextDir nextBeh
    (nextDir, nextBeh) =
      case (curDir, getTrack ts nextCoord) of
        (Up, TurnLeft) -> (Left, curBeh)
        (Up, TurnRight) -> (Right, curBeh)
        (Up, Vertical) -> (Up, curBeh)
        (Left, TurnLeft) -> (Up, curBeh)
        (Left, TurnRight) -> (Down, curBeh)
        (Left, Horizontal) -> (Left, curBeh)
        (Down, TurnLeft) -> (Right, curBeh)
        (Down, TurnRight) -> (Left, curBeh)
        (Down, Vertical) -> (Down, curBeh)
        (Right, TurnLeft) -> (Down, curBeh)
        (Right, TurnRight) -> (Up, curBeh)
        (Right, Horizontal) -> (Right, curBeh)
        (_ , Intersection) -> nextDesc
        (d,t) -> error $ "invalid combination: " ++ show d ++ " and " ++ show t
    (Cart curDir curBeh) = getCart cs coord
    nextCoord =
      case curDir of
        Left -> (x-1,y)
        Right -> (x+1,y)
        Up -> (x,y-1)
        Down -> (x,y+1)
    nextDesc =
      case (curDir, curBeh) of
        (_, []) -> error "no behavior left"
        (d, GoStraight:rest) -> (d, rest)
        (Up, GoLeft:rest) -> (Left, rest)
        (Up, GoRight:rest) -> (Right, rest)
        (Left, GoLeft:rest) -> (Down, rest)
        (Left, GoRight:rest) -> (Up, rest)
        (Right, GoRight:rest) -> (Down, rest)
        (Right, GoLeft:rest) -> (Up, rest)
        (Down, GoLeft:rest) -> (Right, rest)
        (Down, GoRight:rest) -> (Left, rest)



getTrack :: Tracks -> Coord -> Track
getTrack ts c =
  if M.member c ts then ts M.! c
  else error $ "there is no track at " ++ show c


getCart :: Carts -> Coord -> Cart
getCart ts c =
  if cartAt ts c then ts M.! c
  else error $ "there is no cart at " ++ show c


cartAt :: Carts -> Coord -> Bool
cartAt cs c = M.member c cs


empty :: Input
empty = Input M.empty M.empty


parseLines :: String -> Input
parseLines = foldl' parseLine empty . zip [0..] . lines


parseLine :: Input -> (Int, String) -> Input
parseLine inp (y,txt) = foldl' add inp $ zip [0..] txt
  where
    add inp'@(Input cs ts) (x,c) =
      case c of
        '-' -> inp' { tracks = M.insert (x,y) Horizontal ts }
        '|' -> inp' { tracks = M.insert (x,y) Vertical ts }
        '/' -> inp' { tracks = M.insert (x,y) TurnRight ts }
        '\\' -> inp' { tracks = M.insert (x,y) TurnLeft ts }
        '+' -> inp' { tracks = M.insert (x,y) Intersection ts }
        '>' -> inp' { tracks = M.insert (x,y) Horizontal ts, carts = M.insert (x,y) (Cart Right b) cs }
        '<' -> inp' { tracks = M.insert (x,y) Horizontal ts, carts = M.insert (x,y) (Cart Left b) cs }
        '^' -> inp' { tracks = M.insert (x,y) Vertical ts, carts = M.insert (x,y) (Cart Up b) cs }
        'v' -> inp' { tracks = M.insert (x,y) Vertical ts, carts = M.insert (x,y) (Cart Down b) cs }
        ' ' -> inp'
        oth -> error $ "unknown char: " ++ show oth
    b = cycle [ GoLeft, GoStraight, GoRight]


inputTxt :: IO Input
inputTxt = parseLines <$> readFile "./src/Day13/input.txt"

inputTst :: IO Input
inputTst = parseLines <$> readFile "./src/Day13/test.txt"
