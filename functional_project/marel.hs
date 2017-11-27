data Coordinate = Coordinate{ row :: Int, column :: Int} deriving (Show)

cell_to_coord :: String -> Coordinate
cell_to_coord cell_name
    | cell_name == "A1"  || cell_name == "a1" = Coordinate 0 0
    | cell_name == "A2"  || cell_name == "a2" = Coordinate 0 1
    | cell_name == "A3"  || cell_name == "a3" = Coordinate 0 2
    | cell_name == "B1"  || cell_name == "b1" = Coordinate 1 0
    | cell_name == "B1"  || cell_name == "b1" = Coordinate 1 0
    | cell_name == "B2"  || cell_name == "b2" = Coordinate 1 1
    | cell_name == "B3"  || cell_name == "b3" = Coordinate 1 2
    | cell_name == "C1"  || cell_name == "c1" = Coordinate 2 0
    | cell_name == "C2"  || cell_name == "c2" = Coordinate 2 1
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

is_valid_movememet :: String -> String ->[[Char]] -> Bool
is_valid_movememet org_cell des_cell board
    | (row (cell_to_coord org_cell)) == -1 || (column (cell_to_coord org_cell)) == -1  = False
    | (row (cell_to_coord des_cell)) == -1 || (column (cell_to_coord des_cell)) == -1  = False
    | (board !! (row (cell_to_coord org_cell)) !! (column (cell_to_coord org_cell))) == '_' = False
    | otherwise = (is_valid_placement des_cell board)

main :: IO ()
main = do
    shape <- getChar
    let marel_board  = [['X','X','_'],['_','X','_'],['_','X','_']]
    print (check_victory shape marel_board)
    let coord1 = (cell_to_coord "C2")
    print coord1
    print (place_piece 'X' "C3" (place_piece 'X' "C1" marel_board))
    print (move_piece "C2" "B1" marel_board)
    print (is_valid_placement "A5" marel_board)
    print (is_valid_movememet "C2" "B2" marel_board)
