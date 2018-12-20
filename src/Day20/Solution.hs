{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day20.Solution where

import           Data.List (foldl', maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Data.Sequence (Seq((:<|)), (><))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Parsec hiding (Empty)




type Coord = (Int, Int)

data Tile
  = Floor
  | Door
  deriving Eq

type DoorsBetween = Int

type Grid a = Map Coord a


data Direction = West | East | North | South
  deriving (Eq, Show, Ord, Enum, Bounded)


data RegEx
  = Start RegEx
  | Stop
  | Dir Direction RegEx
  | Choice [RegEx] RegEx
  | Empty
  deriving Show

----------------------------------------------------------------------
-- main
run :: IO ()
run = do
  putStrLn "DAY 20"
  regEx <- inputTxt
  let grd = markDoors $ generateGrid regEx

  putStrLn $ "part 1: " ++ show (furthestRoom grd)
  putStrLn $ "part 2: " ++ show (part2 grd)


----------------------------------------------------------------------
-- algorithm

-- | count all rooms with more than 1000 doors to there
-- have to divide by two as I annotate doors too
part2 :: Grid DoorsBetween -> Int
part2 =
  (`div` 2) . length . filter (\(_, doors) -> doors >= 1000) . Map.toList


-- | find the room with maximum 'DoorsBetween'
furthestRoom :: Grid DoorsBetween -> DoorsBetween
furthestRoom =
  snd . maximumBy (comparing snd) . Map.toList

----------------------------------------------------------------------
-- grid generation


-- | visites every room breadth-first and marks the doors in between
markDoors :: Grid Tile -> Grid DoorsBetween
markDoors tileGrid = go Set.empty (Seq.singleton ((0,0), 0)) (Map.singleton (0,0) 0)
  where
    go :: Set Coord -> Seq (Coord, DoorsBetween) -> Grid DoorsBetween -> Grid DoorsBetween
    go _ Seq.Empty grd = grd
    go visited ((nextCoord, doors) :<| restCoords) grd =
      let visited' = Set.insert nextCoord visited
          neighs   = map (updatedDoorCount doors) $ filter (not . (`Set.member` visited)) $ neighbors nextCoord
          restCoords' = restCoords >< Seq.fromList neighs
          grd'     = foldl' (\g (nc, dc) -> Map.insert nc dc g) grd neighs
      in go visited' restCoords' grd'
    neighbors :: Coord -> [Coord]
    neighbors (row,col) =
      filter (`Map.member` tileGrid) $ [ (row-1,col), (row, col-1), (row, col+1), (row+1,col) ]
    updatedDoorCount :: DoorsBetween -> Coord -> (Coord, DoorsBetween)
    updatedDoorCount curCount coord =
      case getTile tileGrid coord of
        Just Door -> (coord, curCount + 1)
        _         -> (coord, curCount)


-- | generates a grid representation from the RegEx
generateGrid :: RegEx -> Grid Tile
generateGrid = hyloRegex fold (Map.fromList [((0,0), Floor)])
  where fold (coord, tile) = Map.insert coord tile


-- | unfolds all coords using the regex and folds an result over those coords and the tile type
-- | the order will be determined by the regex and you shouls make no assumption about it
hyloRegex :: forall state . ((Coord, Tile) -> state -> state) -> state -> RegEx -> state
hyloRegex fold = go (0,0)
  where
    go :: Coord -> state -> RegEx -> state
    go coord@(row,col) state regEx =
      case regEx of
        Start more     -> go coord state more
        Stop           -> state
        Dir North more ->
          let coord'  = (row-1,col)
              coord'' = (row-2,col)
              state'  = fold (coord'', Floor) $ fold (coord', Door) $ state
          in go coord'' state' more
        Dir South more ->
          let coord'  = (row+1,col)
              coord'' = (row+2,col)
              state'  = fold (coord'', Floor) $ fold (coord', Door) $ state
          in go coord'' state' more
        Dir West more ->
          let coord'  = (row,col-1)
              coord'' = (row,col-2)
              state'  = fold (coord'', Floor) $ fold (coord', Door) $ state
          in go coord'' state' more
        Dir East more ->
          let coord'  = (row,col+1)
              coord'' = (row,col+2)
              state'  = fold (coord'', Floor) $ fold (coord', Door) $ state
          in go coord'' state' more
        Choice cs more ->
          let state' = foldl' (go coord) state cs
          in go coord state' more
        Empty -> state


----------------------------------------------------------------------
-- some example

example1 :: [Char]
example1 = "^ENWWW(NEEE|SSE(EE|N))$"
example2 :: [Char]
example2 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
example3 :: [Char]
example3 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"


----------------------------------------------------------------------
-- grid output

drawGrid :: Grid (Tile, DoorsBetween) -> IO ()
drawGrid grd =
  mapM_ (putStrLn . drawLine) [minR..maxR]
  where
    drawLine r = map (drawTile r) [minC..maxC]
    drawTile r c = showTile . fmap fst $ getTile grd (r,c)
    showTile (Just Floor) = '.'
    showTile (Just Door)  = '+'
    showTile Nothing      = '#'
    ((minR,minC), (maxR,maxC)) = bounds grd


----------------------------------------------------------------------
-- helper

getTile :: Grid a -> Coord -> Maybe a
getTile grd crd = Map.lookup crd grd


bounds :: Grid a -> (Coord, Coord)
bounds grd = ((minRow-1, minCol-1), (maxRow+1, maxCol+1))
  where
    minRow = minimum $ map fst coords
    maxRow = maximum $ map fst coords
    minCol = minimum $ map snd coords
    maxCol = maximum $ map snd coords
    coords = Map.keys grd


----------------------------------------------------------------------
-- input

inputTxt :: IO RegEx
inputTxt = parseInput <$> readFile "./src/Day20/input.txt"


----------------------------------------------------------------------
-- parser

parseInput :: String -> RegEx
parseInput = either (error . show) id . parse regExP "input.txt"


type Parser a = Parsec String () a


regExP :: Parser RegEx
regExP = choice [startP, dirP, choiceP, stopP, empty]
  where
    empty   = pure Empty
    startP  = (char '^' *> pure Start) <*> regExP
    stopP   = char '$' *> pure Stop
    dirP    = Dir <$> directionP <*> regExP
    choiceP = Choice <$> (between (char '(') (char ')') ((regExP <|> empty) `sepBy` (char '|'))) <*> regExP


directionP :: Parser Direction
directionP = choice (map mkChoice [('W', West), ('E', East), ('N', North), ('S', South)])
  where mkChoice (c,d) = char c *> pure d
