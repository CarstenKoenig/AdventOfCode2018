module Day13.Solution
  ( run
  ) where

import           Data.List (foldl', sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord (comparing)
import           Data.Tuple (swap)
import           Prelude hiding (Left, Right)


data Input = Input
  { carts  :: Carts
  , tracks :: Tracks
  } deriving Show


type Coord = (Int, Int)


-- | map from coordinates to carts at those
type Carts =
  Map Coord Cart


-- | a cart by it's current direction and
-- pending behavior at intersections:
--    at an intersection the algorithm is supposed
--    to take the head and remove it
data Cart =
  Cart CartDir [AtIntersection]


instance Show Cart where
  show (Cart dir _) = "cart [" ++ show dir ++ "]"


-- | direction a cart faces right now
data CartDir
  = Up
  | Down
  | Left
  | Right
  deriving Show


-- | determines where a cart should move
-- at an intersection
data AtIntersection
  = GoLeft | GoRight | GoStraight
  deriving Show


-- | map from coordinates to the track there
type Tracks =
  Map Coord Track


-- | the direction/layout of a single track piece
data Track
  = Vertical
  | Horizontal
  | TurnLeft
  | TurnRight
  | Intersection
  deriving Show


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 13"
  inp <- inputTxt

  putStrLn $ "part 1: " ++ show (iterateToFirstCollision inp)
  putStrLn $ "part 2: " ++ show (iterateToLastManStanding inp)

----------------------------------------------------------------------
-- Part 1

-- | helper structure
data MoveRes
  -- | continue running from this input
  = Cont Input
  -- | a collision at this coord happened
  | Collision Coord


-- | move all carts till two collide
iterateToFirstCollision :: Input -> Coord
iterateToFirstCollision inp =
  case moveCarts inp (cartCoords inp) of
    Collision at -> at
    Cont inp'    -> iterateToFirstCollision inp'
  where
    moveCarts inp' []     = Cont inp'
    moveCarts inp' (c:cs) =
      case tryMoveCart c inp' of
        Cont inp'' -> moveCarts inp'' cs
        col        -> col


-- | tries to move a single cart
-- if it ends up on a coordinate another cart occupies a 'Collision' is returned
-- otherwise a 'Cont' with the new State
tryMoveCart :: Coord -> Input -> MoveRes
tryMoveCart coord inp@(Input cs _)
  | not isCollision = Cont $ inp { carts = M.insert nextCoord nextCart withOut }
  | otherwise = Collision nextCoord
  where
    withOut = M.delete coord cs
    isCollision = nextCoord `M.member` withOut
    nextCart = Cart nextDir nextBeh
    (nextCoord, nextDir, nextBeh) = moveCart coord inp




----------------------------------------------------------------------
-- part 2


-- | iterates moveing carts where collinding pairs are removed till
-- only one cart is left and return the coord where the survivor is
iterateToLastManStanding :: Input -> Coord
iterateToLastManStanding inp =
  if M.size (carts inp) == 1 then
    head . M.keys $ carts inp
  else
    iterateToLastManStanding $ foldl' (flip moveCartWithoutCol) inp $ cartCoords inp


-- | moves a single Cart - if it collides with another removes both
moveCartWithoutCol :: Coord -> Input -> Input
moveCartWithoutCol coord inp@(Input cs _)
  | not (isCartAt cs coord) = inp
  | not isCollision = inp { carts = M.insert nextCoord nextCart withOut }
  | otherwise = inp { carts = M.delete nextCoord withOut }
  where
    withOut = M.delete coord cs
    isCollision = nextCoord `M.member` withOut
    nextCart = Cart nextDir nextBeh
    (nextCoord, nextDir, nextBeh) = moveCart coord inp


----------------------------------------------------------------------
-- Cart moving

-- | moves a card in the labyrinth based on the problems rules
moveCart :: Coord -> Input -> (Coord, CartDir, [AtIntersection])
moveCart coord@(x,y) (Input cs ts) =
  case (curDir, getTrack ts nextCoord) of
    (Up, TurnLeft)      -> (nextCoord, Left, curBeh)
    (Up, TurnRight)     -> (nextCoord, Right, curBeh)
    (Up, Vertical)      -> (nextCoord, Up, curBeh)
    (Left, TurnLeft)    -> (nextCoord, Up, curBeh)
    (Left, TurnRight)   -> (nextCoord, Down, curBeh)
    (Left, Horizontal)  -> (nextCoord, Left, curBeh)
    (Down, TurnLeft)    -> (nextCoord, Right, curBeh)
    (Down, TurnRight)   -> (nextCoord, Left, curBeh)
    (Down, Vertical)    -> (nextCoord, Down, curBeh)
    (Right, TurnLeft)   -> (nextCoord, Down, curBeh)
    (Right, TurnRight)  -> (nextCoord, Up, curBeh)
    (Right, Horizontal) -> (nextCoord, Right, curBeh)
    (_ , Intersection)  -> nextDesc
    (d,t) -> error $ "invalid combination: " ++ show d ++ " and " ++ show t
  where
    (Cart curDir curBeh) = getCart cs coord
    nextCoord =
      case curDir of
        Left  -> (x-1,y)
        Right -> (x+1,y)
        Up    -> (x,y-1)
        Down  -> (x,y+1)
    nextDesc =
      case (curDir, curBeh) of
        (_, []) -> error "no behavior left"
        (d, GoStraight:rest)  -> (nextCoord, d, rest)
        (Up, GoLeft:rest)     -> (nextCoord, Left, rest)
        (Up, GoRight:rest)    -> (nextCoord, Right, rest)
        (Left, GoLeft:rest)   -> (nextCoord, Down, rest)
        (Left, GoRight:rest)  -> (nextCoord, Up, rest)
        (Right, GoRight:rest) -> (nextCoord, Down, rest)
        (Right, GoLeft:rest)  -> (nextCoord, Up, rest)
        (Down, GoLeft:rest)   -> (nextCoord, Right, rest)
        (Down, GoRight:rest)  -> (nextCoord, Left, rest)


----------------------------------------------------------------------
-- helpers

getTrack :: Tracks -> Coord -> Track
getTrack ts c =
  if M.member c ts then ts M.! c
  else error $ "there is no track at " ++ show c


getCart :: Carts -> Coord -> Cart
getCart ts c =
  if isCartAt ts c then ts M.! c
  else error $ "there is no cart at " ++ show c


isCartAt :: Carts -> Coord -> Bool
isCartAt cs c = M.member c cs


cartCoords :: Input -> [Coord]
cartCoords = sortBy (comparing swap) . M.keys . carts


empty :: Input
empty = Input M.empty M.empty


----------------------------------------------------------------------
-- Parsing

parseLines :: String -> Input
parseLines = foldl' parseLine empty . zip [0..] . lines


parseLine :: Input -> (Int, String) -> Input
parseLine inp (y,txt) = foldl' add inp $ zip [0..] txt
  where
    add inp'@(Input cs ts) (x,c) =
      case c of
        '-'  -> inp' { tracks = M.insert (x,y) Horizontal ts   }
        '|'  -> inp' { tracks = M.insert (x,y) Vertical ts     }
        '/'  -> inp' { tracks = M.insert (x,y) TurnRight ts    }
        '\\' -> inp' { tracks = M.insert (x,y) TurnLeft ts     }
        '+'  -> inp' { tracks = M.insert (x,y) Intersection ts }
        '>'  -> inp' { tracks = M.insert (x,y) Horizontal ts   , carts = M.insert (x,y) (Cart Right b) cs }
        '<'  -> inp' { tracks = M.insert (x,y) Horizontal ts   , carts = M.insert (x,y) (Cart Left b) cs }
        '^'  -> inp' { tracks = M.insert (x,y) Vertical ts     , carts = M.insert (x,y) (Cart Up b) cs }
        'v'  -> inp' { tracks = M.insert (x,y) Vertical ts     , carts = M.insert (x,y) (Cart Down b) cs }
        ' '  -> inp'
        oth  -> error $ "unknown char: " ++ show oth
    b = cycle [ GoLeft, GoStraight, GoRight]


----------------------------------------------------------------------
-- IO

inputTxt :: IO Input
inputTxt = parseLines <$> readFile "./src/Day13/input.txt"
