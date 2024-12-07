import Data.Function
import Data.Set qualified as Set

type Room = [[Char]]

type Vec2 = (Int, Int)

type Position = Vec2

type Direction = Vec2

type Size = Vec2

findGuard :: Room -> Position
findGuard room =
  [ (row_i, col_i)
    | (row_i, row) <- zip [0 ..] room,
      (col_i, cell) <- zip [0 ..] row,
      cell `elem` ['<', '>', '^', 'v']
  ]
    & head

up :: Direction
up = (-1, 0)

down :: Direction
down = (1, 0)

left :: Direction
left = (0, -1)

right :: Direction
right = (0, 1)

getGuardDirection :: Room -> Position -> Direction
getGuardDirection room (r, c) = case (room !! r) !! c of
  '>' -> right
  '<' -> left
  'v' -> down
  '^' -> up

turnGuardDirection :: Direction -> Direction
turnGuardDirection dir
  | dir == up = right
  | dir == right = down
  | dir == down = left
  | dir == left = up
  | otherwise = up

move :: Direction -> Position -> Position
move (dr, dc) (r, c) = (r + dr, c + dc)

getRoomSize :: Room -> Size
getRoomSize room = (length room, length (head room))

isFacingBorder :: Room -> Direction -> Position -> Bool
isFacingBorder room (dr, dc) (r, c) =
  let (new_r, new_c) = (r + dr, c + dc)
      (rows, cols) = getRoomSize room
   in new_r < 0 || new_c < 0 || new_r >= rows || new_c >= cols

isFacingObstacle :: Room -> Direction -> Position -> Bool
isFacingObstacle room (dr, dc) (r, c) =
  let (new_r, new_c) = (r + dr, c + dc)
   in ((room !! new_r) !! new_c) == '#'

findPath :: Room -> Direction -> Position -> Set.Set Position -> Set.Set Position
findPath room dir pos visited
  | isFacingBorder room dir pos = Set.insert pos visited
  | otherwise = findPath room new_dir new_pos new_visited
  where
    new_dir =
      if isFacingObstacle room dir pos
        then turnGuardDirection dir
        else dir
    new_pos = move new_dir pos
    new_visited = Set.insert pos visited

main = do
  input_str <- readFile "input.txt"

  let room = lines input_str
  let guard_pos = findGuard room
  let guard_dir = getGuardDirection room guard_pos
  let path = findPath room guard_dir guard_pos Set.empty

  print $ length path
