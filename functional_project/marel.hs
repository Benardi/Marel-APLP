check_left_diagonal :: Char -> [[Char]] -> Bool
check_left_diagonal shape board = do
  if(((board !! 0) !! 0 == shape) && ((board !! 1) !! 1 == shape) && ((board !! 2) !! 2 == shape))
    then True
  else False

check_right_diagonal :: Char -> [[Char]] -> Bool
check_right_diagonal shape board = do
  if(((board !! 0) !! 2 == shape) && ((board !! 1) !! 1 == shape) && ((board !! 2) !! 0 == shape))
    then True
  else False

check_all_diagonals :: Char -> [[Char]] -> Bool
check_all_diagonals shape board = do
  if((check_left_diagonal shape board) || (check_right_diagonal shape board))
    then True
  else False


check_row :: Char -> Int -> [[Char]] -> Bool
check_row shape level board = do
  if((board !! level) == [shape, shape, shape])
    then True
  else False

check_all_rows :: Char -> [[Char]] -> Bool
check_all_rows shape board = do
  if((check_row shape 0 board) || (check_row shape 1 board) || (check_row shape 2 board))
    then True
  else False

check_column :: Char -> Int -> [[Char]] -> Bool
check_column shape level board = do
  if(((board !! 0) !! level == shape) && ((board !! 1) !! level == shape) && ((board !! 2) !! level == shape))
    then True
  else False

check_all_columns :: Char -> [[Char]] -> Bool
check_all_columns shape board = do
  if((check_column shape 0 board) || (check_column shape 1 board) || (check_column shape 2 board))
    then True
  else False


check_victory :: Char -> [[Char]] -> Bool
check_victory shape board = do
  if ((check_all_diagonals shape board) || (check_all_rows shape board) || (check_all_columns shape board))
    then True
  else False

create_board :: [Char] -> [Char] -> [Char]-> [[Char]]
create_board row1 row2 row3 = [row1,row2, row3]


slice from to xs = take (to - from + 1) (drop from xs)

addVal :: [Char] -> [Char] -> [Char]-> [[Char]]
addVal row1 row2 row3 = [row1,row2, row3]

place_piece :: Char -> Int -> Int -> [[Char]] -> [[Char]]
place_piece shape row column board = do
    let flat = [if x == row && y == column then shape else ((board !! x) !! y) | x <- [0..2], y <- [0..2]]
    addVal (slice 0 2 flat) (slice 3 5 flat) (slice 6 8 flat)

main :: IO ()
main = do
    shape <- getChar
    let marel_board  = [['X','X','_'],['_','X','_'],['_','_','_']]
    print (check_victory shape marel_board)
    print (place_piece 'X' 2 1 marel_board)
