{-# LANGUAGE TupleSections #-}
module Day17.Solution
  ( run
  ) where

import           Data.Char (isDigit)
import           Data.Ix (inRange)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Parsec
import Data.List (foldl')


type Bounds = (Int, Int)
type Coord = (Int, Int)

type World = Map Coord Content

data Content
  = Clay
  | Wet
  | Water
  deriving (Show, Eq)


----------------------------------------------------------------------
-- main

run :: IO ()
run = do
  putStrLn "DAY 17"
  world <- fillWorld <$> inputTxt


  putStrLn $ "part 1: " ++ show (part1 world)
  putStrLn $ "part 2: " ++ show (part2 world)


-- | part 1: count all non-clay tiles (those are either wet or water)
part1 :: World -> Int
part1 world =
  let notClay = M.filter (/= Clay) world
  in M.size notClay


-- | part 2: count only the water tiles
part2 :: World -> Int
part2 world =
  let waterTiles = M.filter (== Water) world
  in M.size waterTiles


----------------------------------------------------------------------
-- algorithm


-- | fill the map beginning from the foundain at 500 and the given ranges depth
fillWorld :: World -> World
fillWorld world = flowDown bounds (500, yMin) world
  where bounds@(yMin,_) = getBounds world


-- | flow down while empty
-- flow horizontally if hit Clay or Water
-- stop if already seen
flowDown :: Bounds -> Coord -> World -> World
flowDown bounds cur@(x,y) !world
  -- if we went outside the range stop
  | not (inRange bounds y) = world
  | otherwise =
    case M.lookup cur world of
      -- if there is nothing here continue and mark this spot
      Nothing    -> flowDown bounds (x,y+1) (M.insert cur Wet $ world)
      -- if we hit Clay or Water (can happen because we go depth-first) switch to Horizontal
      Just Clay  -> flowHorizontal bounds (x,y-1) world
      Just Water -> flowHorizontal bounds (x,y-1) world
      -- if we hit a Wet spot we can stop as we already went here before
      Just Wet   -> world


flowHorizontal :: Bounds -> Coord -> World -> World
flowHorizontal !bounds (!x,!y) !world
  -- if we went outside the range stop (can happen because this will go up again)
  | not (inRange bounds y) = world
  | otherwise =
    -- follow a possible plateau here left then right, recurively going down if needed
    let (!hitLeft, worldL) = flowLeft bounds (x,y) world
        (!hitRight, worldR) = flowRight bounds (x,y) worldL
  in case (hitLeft, hitRight) of
    -- if left and right hit a clay-wall
    (Just from, Just to) ->
      -- we found a water pduddle - mark it:
      let world' = foldl' (\w x' -> M.insert (x',y) Water w) worldR [from..to]
      -- and continue upwards
      in flowHorizontal bounds (x,y-1) world'
    -- if either side had no wall we are done as the branches did continue recursively
    _ -> worldR


-- | helper to flow horizontally to the left
flowLeft :: Bounds -> Coord -> World -> (Maybe Int, World)
flowLeft bounds = flowHor bounds (-1)


-- | helper to flow horizontally to the right
flowRight :: Bounds -> Coord -> World -> (Maybe Int, World)
flowRight bounds = flowHor bounds 1


-- | flow horizontally in a direction given by 'dX' (either -1 or 1)
flowHor :: Bounds -> Int -> Coord -> World -> (Maybe Int, World)
flowHor bounds !dX cur@(!x,!y) !world
  -- if we hit a clay-wall stop here and return it's pos
  | isClay world cur               = (Just (x-dX), world)
  -- if we hit water (another branch is looking here) we can stop - the other branch will handle it
  | isWater world cur              = (Nothing, world)
  -- if bellow is a wet-spot we went here before
  | isWet world (x,y+1)            = (Nothing, M.insert cur Wet world)
  -- if bellow is empty we found a hole and can start going down again - there will be no wall
  | isEmpty world (x,y+1)          = (Nothing, flowDown bounds (x,y+1) (M.insert cur Wet world))
  -- otherwise continue moving horizontally
  | otherwise                      = flowHor bounds dX (x+dX,y) (M.insert cur Wet world)



----------------------------------------------------------------------
-- helpers

isTile :: Maybe Content -> World -> Coord -> Bool
isTile tile world coord =
  M.lookup coord world == tile


isClay :: World -> Coord -> Bool
isClay = isTile (Just Clay)


isWater :: World -> Coord -> Bool
isWater = isTile (Just Water)


isWet :: World -> Coord -> Bool
isWet = isTile (Just Wet)


isEmpty :: World -> Coord -> Bool
isEmpty = isTile Nothing


toWorld :: Set Coord -> World
toWorld = M.fromList . map (, Clay) . S.toList


getBounds :: World -> Bounds
getBounds world =
  let y0 = minimum ys
      y1 = maximum ys
  in (y0,y1)
  where ys = map (snd . fst) $ coords
        coords = M.toList world


----------------------------------------------------------------------
-- IO

inputTxt :: IO World
inputTxt = toWorld . parseInput <$> readFile "./src/Day17/input.txt"


----------------------------------------------------------------------
-- parsing

parseInput :: String -> Set Coord
parseInput = either (error . show) id . parse inputP "input.txt"


type Parser a = Parsec String () a


inputP :: Parser (Set Coord)
inputP = S.fromList . concat <$> (many $ (coordP <|> coordP') <* many newline)


coordP :: Parser [Coord]
coordP = do
  xs <- coordCompP 'x'
  _ <- string ", "
  ys <- coordCompP 'y'
  pure $ [ (x,y) | x <- xs, y <- ys ]


coordP' :: Parser [Coord]
coordP' = do
  ys <- coordCompP 'y'
  _ <- string ", "
  xs <- coordCompP 'x'
  pure $ [ (x,y) | x <- xs, y <- ys ]


coordCompP :: Char -> Parser [Int]
coordCompP c = char c *> char '=' *> rangeP


rangeP :: Parser [Int]
rangeP = do
  start <- numP
  maybe [start] (\end -> [start..end]) <$> (optionMaybe $ do
    _ <- string ".."
    numP)


numP :: Parser Int
numP = read <$> many1 (satisfy isDigit)


