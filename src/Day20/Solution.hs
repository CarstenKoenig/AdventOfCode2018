{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
module Day20.Solution where

import           Data.List (foldl', maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Text.Parsec hiding (Empty)
import Debug.Trace (trace)




type Coord = (Int, Int)

data Tile
  = Floor
  | Door

type DoorsBetween = Int

type Grid = Map Coord (Tile, DoorsBetween)


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
  let grd = generateGrid regEx

  putStrLn $ "part 1: " ++ show (furthestRoom grd)
  putStrLn $ "part 2: " ++ show (part2 grd)


----------------------------------------------------------------------
-- algorithm

-- | count all rooms with more than 1000 doors to there
-- have to divide by two as I annotate doors too
part2 :: Grid -> Int
part2 =
  (`div` 2) . length . filter (\(_, (_,doors)) -> doors >= 1000) . Map.toList

-- | find the room with maximum 'DoorsBetween'
furthestRoom :: Grid -> (Coord, DoorsBetween)
furthestRoom =
  (\(crd, (_, doors)) -> (crd, doors)) . maximumBy (comparing (snd .snd)) . Map.toList


----------------------------------------------------------------------
-- grid generation

-- | generates the grid from the regex
generateGrid :: RegEx -> Grid
generateGrid = go (0,0) 0 (Map.fromList [((0,0), (Floor, 0))])
  where
    go :: Coord -> DoorsBetween -> Grid -> RegEx -> Grid
    go coord@(row,col) nrDoors grd regEx =
      case regEx of
        Start more     -> go coord nrDoors grd more
        Stop           -> grd
        Dir North more ->
          let coord'  = (row-1,col)
              coord'' = (row-2,col)
              grd'   = Map.insertWith minDoors coord' (Door, nrDoors+1) $ Map.insertWith minDoors coord'' (Floor, nrDoors+1) grd
          in go coord'' (nrDoors+1)  grd' more
        Dir South more ->
          let coord'  = (row+1,col)
              coord'' = (row+2,col)
              grd'   = Map.insertWith minDoors coord' (Door, nrDoors+1) $ Map.insertWith minDoors coord'' (Floor, nrDoors+1) grd
          in go coord'' (nrDoors+1) grd' more
        Dir West more ->
          let coord'  = (row,col-1)
              coord'' = (row,col-2)
              grd'   = Map.insertWith minDoors coord' (Door, nrDoors+1) $ Map.insertWith minDoors coord'' (Floor, nrDoors+1) grd
          in go coord'' (nrDoors+1) grd' more
        Dir East more ->
          let coord'  = (row,col+1)
              coord'' = (row,col+2)
              grd'   = Map.insertWith minDoors coord' (Door, nrDoors+1) $ Map.insertWith minDoors coord'' (Floor, nrDoors+1) grd
          in go coord'' (nrDoors+1) grd' more
        Choice cs more ->
          let grd' = foldl' (go coord nrDoors) grd cs
          in go coord nrDoors grd' more
        Empty -> grd
    minDoors (newTile, newDoors) (_, oldDoors) =
      let updDoors = (if oldDoors /= newDoors then trace ("updating doors") else id) min newDoors oldDoors
      in (newTile, updDoors)


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

drawGrid :: Grid -> IO ()
drawGrid grd =
  mapM_ (putStrLn . drawLine) [minR..maxR]
  where
    drawLine r = map (drawTile r) [minC..maxC]
    drawTile r c = showTile $ getTile grd (r,c)
    showTile (Just Floor) = '.'
    showTile (Just Door)  = '+'
    showTile Nothing      = '#'
    ((minR,minC), (maxR,maxC)) = bounds grd


----------------------------------------------------------------------
-- helper

getTile :: Grid -> Coord -> Maybe Tile
getTile grd crd = fst <$> Map.lookup crd grd


bounds :: Grid -> (Coord, Coord)
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
