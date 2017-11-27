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

place_piece :: Char -> Int -> Int -> [[Char]] -> [[Char]]
place_piece shape row column board = do
    let flat = [if x == row && y == column then shape else ((board !! x) !! y) | x <- [0..2], y <- [0..2]]
    create_board (slice 0 2 flat) (slice 3 5 flat) (slice 6 8 flat)

is_valid_placement :: Int -> Int -> [[Char]] -> Bool
is_valid_placement row column board
    | row > 2 || row < 0 = False
    | column > 2 || column < 0 = False
    | (board !! row !! column) /= '_' = False
    | otherwise = True

move_piece :: Int -> Int -> Int -> Int ->[[Char]] -> [[Char]]
move_piece row_org column_org row_des column_des board = do
      (place_piece '_' row_org column_org (place_piece (board !! row_org !! column_org) row_des column_des board))

is_valid_movememet :: Int -> Int -> Int -> Int ->[[Char]] -> Bool
is_valid_movememet row_org column_org row_des column_des board
    | row_org > 2 || row_org < 0  = False
    | column_org > 2 || column_org < 0  = False
    | (board !! row_org !! column_org) == '_' = False
    | otherwise = (is_valid_placement row_des column_des board)


main :: IO ()
main = do
    shape <- getChar
    let marel_board  = [['X','X','_'],['_','X','_'],['_','X','_']]
    print (check_victory shape marel_board)
    print (place_piece 'X' 2 2 (place_piece 'X' 2 0 marel_board))
    print (move_piece 2 1 1 0 marel_board)
    print (is_valid_placement 2 3 marel_board)
    print (is_valid_movememet 2 1 1 1 marel_board)
