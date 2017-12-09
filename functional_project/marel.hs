data Coordinate = Coordinate{ row :: Int, column :: Int} deriving (Show)

cell_to_coord :: String -> Coordinate
cell_to_coord cell_name
    | cell_name == "A1"  || cell_name == "a1" = Coordinate 0 0
    | cell_name == "A2"  || cell_name == "a2" = Coordinate 1 0
    | cell_name == "A3"  || cell_name == "a3" = Coordinate 2 0
    | cell_name == "B1"  || cell_name == "b1" = Coordinate 0 1
    | cell_name == "B2"  || cell_name == "b2" = Coordinate 1 1
    | cell_name == "B3"  || cell_name == "b3" = Coordinate 2 1
    | cell_name == "C1"  || cell_name == "c1" = Coordinate 0 2
    | cell_name == "C2"  || cell_name == "c2" = Coordinate 1 2
    | cell_name == "C3"  || cell_name == "c3" = Coordinate 2 2
    | otherwise = Coordinate (-1) (-1)

check_left_diagonal :: Char -> [[Char]] -> Bool
check_left_diagonal shape board = do
  (((board !! 0) !! 0 == shape) && ((board !! 1) !! 1 == shape) && ((board !! 2) !! 2 == shape))

check_right_diagonal :: Char -> [[Char]] -> Bool
check_right_diagonal shape board = do
  (((board !! 0) !! 2 == shape) && ((board !! 1) !! 1 == shape) && ((board !! 2) !! 0 == shape))


check_all_diagonals :: Char -> [[Char]] -> Bool
check_all_diagonals shape board = do
  ((check_left_diagonal shape board) || (check_right_diagonal shape board))

check_row :: Char -> Int -> [[Char]] -> Bool
check_row shape level board = do
  ((board !! level) == [shape, shape, shape])

check_all_rows :: Char -> [[Char]] -> Bool
check_all_rows shape board = do
  ((check_row shape 0 board) || (check_row shape 1 board) || (check_row shape 2 board))


check_column :: Char -> Int -> [[Char]] -> Bool
check_column shape level board = do
  (((board !! 0) !! level == shape) && ((board !! 1) !! level == shape) && ((board !! 2) !! level == shape))

check_all_columns :: Char -> [[Char]] -> Bool
check_all_columns shape board = do
  ((check_column shape 0 board) || (check_column shape 1 board) || (check_column shape 2 board))

check_victory :: Char -> [[Char]] -> Bool
check_victory shape board = do
  ((check_all_diagonals shape board) || (check_all_rows shape board) || (check_all_columns shape board))


create_board :: [Char] -> [Char] -> [Char]-> [[Char]]
create_board row1 row2 row3 = [row1,row2, row3]

slice from to xs = take (to - from + 1) (drop from xs)

place_piece :: Char -> String -> [[Char]] -> [[Char]]
place_piece shape cell_name board = do
    let coord = (cell_to_coord cell_name)
    let flat = [if x == (row coord) && y == (column coord) then shape else ((board !! x) !! y) | x <- [0..2], y <- [0..2]]
    create_board (slice 0 2 flat) (slice 3 5 flat) (slice 6 8 flat)

is_valid_placement :: String -> [[Char]] -> Bool
is_valid_placement cell board
    | (row (cell_to_coord cell)) == -1 || (column (cell_to_coord cell)) == -1 = False
    | (board !! (row (cell_to_coord cell)) !! (column (cell_to_coord cell))) /= '_' = False
    | otherwise = True

move_piece :: String -> String ->[[Char]] -> [[Char]]
move_piece org_cell des_cell board = do
      let org_coord = cell_to_coord org_cell
      (place_piece '_' org_cell (place_piece (board !! (row org_coord) !! (column org_coord)) des_cell board))

is_valid_movement_orig :: String -> Char ->[[Char]] -> Bool
is_valid_movement_orig org_cell shape board
    | (row (cell_to_coord org_cell)) == -1 || (column (cell_to_coord org_cell)) == -1  = False
    | (board !! (row (cell_to_coord org_cell)) !! (column (cell_to_coord org_cell))) /= shape = False
    | otherwise = True

receive_placement board = do
  coord <- getLine
  if is_valid_placement coord board
    then return coord
  else do
    putStrLn("\nPlease choose a valid coordinate for your placement.")
    receive_placement board

receive_movement board shape = do
  org_cell <- getLine
  if is_valid_movement_orig org_cell shape board
    then return org_cell
  else do
    putStrLn("\nPlease choose a valid coordinate for your movement.")
    receive_movement board shape

placementRound 0  board shape1 shape2 = return board
placementRound n board shape1 shape2 = do
  putStrLn("\nPlayer 1, Please choose a coordinate to place your cell.")
  coord1 <- receive_placement board
  let board_plcm1 = (place_piece shape1 coord1 board)
  snapshot_board board_plcm1
  putStrLn("\nPlayer 2, Please choose a coordinate to place your cell.")
  coord2 <- receive_placement board_plcm1
  let board_plcm2 = (place_piece shape2 coord2 board_plcm1)
  snapshot_board board_plcm2
  placementRound (n-1) board_plcm2 shape1 shape2

movementRound board shape1 shape2 = do
  putStrLn("\nPlayer 1, Please choose a piece to be moved.")
  coord1_from <- receive_movement board shape1
  putStrLn("\nPlayer 1, Please choose to where it should be moved.")
  coord1_to <- receive_placement board

  let board_mvm1 = (move_piece coord1_from coord1_to board)
  snapshot_board board_mvm1
  if check_victory shape1 board_mvm1
    then putStrLn("\n\tPlayer 1 has won\n")
  else do
    putStrLn("\nPlayer 2, Please choose a piece to be moved.")
    coord2_from <- receive_movement board_mvm1 shape2
    putStrLn("\nPlayer 2, Please choose to where it should be moved.")
    coord2_to <- receive_placement board_mvm1
    let board_mvm2 = (move_piece coord2_from coord2_to board_mvm1)
    snapshot_board board_mvm2
    if check_victory shape2 board_mvm2
      then putStrLn("\n\tPlayer 2 has won\n")
    else movementRound board_mvm2 shape1 shape2

snapshot_board :: [[Char]] -> IO ()
snapshot_board board = do
  putStrLn("\n    A  B  C")
  putStrLn(" 1  " ++ [((board !! 0) !! 0)] ++ "  " ++ [((board !! 0) !! 1)] ++ "  " ++ [((board !! 0) !! 2)])
  putStrLn(" 2  " ++ [((board !! 1) !! 0)] ++ "  " ++ [((board !! 1) !! 1)] ++ "  " ++ [((board !! 1) !! 2)])
  putStrLn(" 3  " ++ [((board !! 2) !! 0)] ++ "  " ++ [((board !! 2) !! 1)] ++ "  " ++ [((board !! 2) !! 2)]++ "\n")
  return()

main :: IO ()
main = do
    let marel_board  = [['_','_','_'],['_','_','_'],['_','_','_']]
    snapshot_board marel_board

    putStrLn("\nPlayer 1, please choose the shape of your piece.")
    shape1 <- getChar
    getLine -- cleans buffer
    putStrLn("\nPlayer 2, please choose the shape of your piece.")
    shape2 <- getChar
    getLine -- cleans buffer
    snapshot_board marel_board

    board_past_placement <- placementRound 3 marel_board shape1 shape2
    snapshot_board board_past_placement

    if check_victory shape1 board_past_placement
      then putStrLn("\n\tPlayer 1 has won\n")
    else do
      if check_victory shape2 board_past_placement
        then putStrLn("\n\tPlayer 2 has won\n")
      else movementRound board_past_placement shape1 shape2

    return()
