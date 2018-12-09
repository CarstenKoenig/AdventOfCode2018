module Day9.Solution where

import           Control.Applicative (Const(..))
import           Data.List (foldl')
import           Data.List.PointedList (PointedList)
import           Data.List.PointedList.Circular as PLC
import           Data.Maybe (fromJust)
import           Utils.Counter (Counter)
import qualified Utils.Counter as C

run :: IO ()
run = do
  putStrLn "DAY 9"
  putStrLn $ "part 1: " ++ show part1
  putStrLn $ "part 2: " ++ show part2


part1 :: Int
part1 = snd . highScore . players $ play 425 70848


part2 :: Int
part2 = snd . highScore . players $ play 425 (70848 * 100)


----------------------------------------------------------------------
-- algorithm

type Marble = Int

type Circle = PointedList Int

data State = State
  { circle  :: !Circle
  , players :: Players
  , turn    :: Player
  } deriving Show


-- | plays a game with @nrP players where @nrM marbles are inserted
play :: Player -> Marble -> State
play nrP nrM =
  foldl' (flip move) (start nrP) [1..nrM]


-- | initializes the game
start :: Int -> State
start nrPlayers = State
  { circle  = PLC.singleton 0
  , players = initPlayers nrPlayers
  , turn    = 1
  }


-- | a move - if the marbles value is divisible by 23 a @scoreMove is used
-- if not a @insertMove
move :: Marble -> State -> State
move m st
  | m `mod` 23 == 0 = scoreMove m st
  | otherwise       = insertMove m st


-- | normal move: just inserts the marble 2 positions to the right
insertMove :: Marble -> State -> State
insertMove m st =
  st { circle = PLC.insertRight m . PLC.next $ circle st
     , turn    = nextTurn st
     }


-- | instead of inserting the marble it is counted as score to the current player
-- additional to the rules of the game an additional marble is taken away 7 left of the
-- current which is added as score as well
scoreMove :: Marble -> State -> State
scoreMove m st =
  let circle' = PLC.moveN (-7) (circle st)
      win = getConst $ PLC.focus Const circle'
  in st { circle = fromJust $ PLC.deleteRight circle'
        , players = scorePlayer (turn st) (m + win) (players st)
        , turn    = nextTurn st
        }


-- | determins the next player (just wraps around to 1 at the end)
nextTurn :: State -> Player
nextTurn st = turn st `mod` (C.size $ players st) + 1

----------------------------------------------------------------------
-- Players and Points

type Players = Counter Player Points
type Player = Int
type Points = Int


-- | highscore: player with the highest points
highScore :: Players -> (Player, Points)
highScore = C.maximum


-- | adds score to a player
scorePlayer :: Player -> Points -> Players -> Players
scorePlayer pl pts =
  C.add pl pts


-- | inits players all with score 0
initPlayers :: Int -> Players
initPlayers n = C.fromList [ (i,0) | i <- [1..n] ]
