data Coordinate = Coordinate{ row :: Int, column :: Int} deriving (Show)
data Player = Player{ name :: String } deriving (Show)

playerName :: Player -> String
playerName (Player name) = name

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

welcoming_screen = do
    putStrLn("\t################################################################")
    putStrLn("\t#                MAREL - GAME OF THE THREE TRAILS              #")
    putStrLn("\t#                   Play and have lots of fun                  #")
    putStrLn("\t#                                                              #")
    putStrLn("\t################################################################")
    putStrLn("\nChoose an option:")
    putStrLn("Option (1): play with a friend.")
    putStrLn("Option (2): play with the computer.")
    putStrLn("Option (anything): quit the game.")
    putStrLn("Option is:")
    return()

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

is_valid_moviment ::  String -> String -> [[Char]] -> Bool 
is_valid_moviment coord_from coord_to board
    | not (is_valid_placement coord_to board) = False
    | (abs((row (cell_to_coord coord_from)) - (row (cell_to_coord coord_to))) == 1) && ((column (cell_to_coord coord_from)) - (column (cell_to_coord coord_to)) == 0) = True
    | (abs((column (cell_to_coord coord_from)) - (column (cell_to_coord coord_to))) == 1) && ((row (cell_to_coord coord_from)) - (row (cell_to_coord coord_to)) == 0) = True
    | (column (cell_to_coord coord_to) == 1) && (row (cell_to_coord coord_to) == 1) = True
    | (column (cell_to_coord coord_from) == 1) && (row (cell_to_coord coord_from) == 1) = True
    | otherwise = False

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

placementRound 0  board player1 player2 shape1 shape2 = return board
placementRound n board player1 player2 shape1 shape2 = do
  putStrLn("\n" ++ (playerName player1) ++ ", Please choose a coordinate to place your cell.")
  coord1 <- receive_placement board
  let board_plcm1 = (place_piece shape1 coord1 board)
  snapshot_board board_plcm1
  
  if not (check_victory shape1 board_plcm1) then do
    putStrLn("\n" ++ (playerName player2) ++ ", Please choose a coordinate to place your cell.")
    coord2 <- receive_placement board_plcm1
    let board_plcm2 = (place_piece shape2 coord2 board_plcm1)
    snapshot_board board_plcm2
    placementRound (n-1) board_plcm2 player1 player2 shape1 shape2
  else return board_plcm1 

movementRound board player1 player2 shape1 shape2 = do
  putStrLn("\n" ++ (playerName player1) ++ ", Please choose a piece to be moved.")
  coord1_from <- receive_movement board shape1
  putStrLn("\n" ++ (playerName player1) ++ ", Please choose to where it should be moved.")
  coord1_to <- getLine

  if is_valid_moviment coord1_from coord1_to board then do
    let board_mvm1 = (move_piece coord1_from coord1_to board)
    snapshot_board board_mvm1
    if check_victory shape1 board_mvm1
        then putStrLn("\n" ++ (playerName player1) ++ " has won\n")
    else movementRoundPlayerTwo board_mvm1 player1 player2 shape1 shape2
  else do
    putStrLn("\nInvalid move for player one, please choose a valid movement.")
    movementRound board player1 player2 shape1 shape2

movementRoundPlayerTwo board player1 player2 shape1 shape2 = do
  putStrLn("\n" ++ (playerName player2) ++ ", Please choose a piece to be moved.")
  coord2_from <- receive_movement board shape2
  putStrLn("\n" ++ (playerName player2) ++ ", Please choose to where it should be moved.")
  coord2_to <- getLine

  if is_valid_moviment coord2_from coord2_to board then do
    let board_mvm2 = (move_piece coord2_from coord2_to board)
    snapshot_board board_mvm2
    if check_victory shape2 board_mvm2
        then putStrLn("\n" ++ (playerName player2) ++ " has won\n")
    else movementRound board_mvm2 player1 player2 shape1 shape2
  else do
    putStrLn("\nInvalid move for player two, please choose a valid movement.")
    movementRoundPlayerTwo board player1 player2 shape1 shape2

selectShape2 shape1 = do
  shapeTwo <- getChar
  getLine -- cleans buffer

  if shapeTwo == shape1 
  	then do
  		putStrLn("Letter already in use, please select another letter") 
  		selectShape2 shape1
  else do
  	putStrLn("Letter Selected :) ")
	return shapeTwo



snapshot_board :: [[Char]] -> IO ()
snapshot_board board = do
  putStrLn("\n    A  B  C")
  putStrLn(" 1  " ++ [((board !! 0) !! 0)] ++ "  " ++ [((board !! 0) !! 1)] ++ "  " ++ [((board !! 0) !! 2)])
  putStrLn(" 2  " ++ [((board !! 1) !! 0)] ++ "  " ++ [((board !! 1) !! 1)] ++ "  " ++ [((board !! 1) !! 2)])
  putStrLn(" 3  " ++ [((board !! 2) !! 0)] ++ "  " ++ [((board !! 2) !! 1)] ++ "  " ++ [((board !! 2) !! 2)]++ "\n")
  return()

main :: IO ()
main = do
    welcoming_screen
    option <- getLine
    
    if option == "1" || option == "2" then do
        let marel_board  = [['_','_','_'],['_','_','_'],['_','_','_']]
        snapshot_board marel_board

        putStrLn("\nPlayer one choose the name of your player: ")
        name <- getLine
        let player1 = Player name
        putStrLn("\n" ++ (playerName player1) ++ ", please choose the shape of your piece.")
        shape1 <- getChar
        getLine -- cleans buffer
        
        if option == "1" then do
            putStrLn("\nPlayer two choose the name of your player: ")
            name <- getLine
            let player2 = Player name 
            putStrLn("\n" ++ (playerName player2) ++ ", please choose the shape of your piece.")
            shape2 <- selectShape2 shape1
            
            snapshot_board marel_board

            board_past_placement <- placementRound 3 marel_board player1 player2 shape1 shape2
            snapshot_board board_past_placement

            if check_victory shape1 board_past_placement
               then putStrLn((playerName player1) ++ " has won\n")
            else do
               if check_victory shape2 board_past_placement
                   then putStrLn((playerName player2) ++ " has won\n")
               else movementRound board_past_placement player1 player2 shape1 shape2
        else putStrLn("\nMovement of the computer in the implementation phase") -- Missing the implementation
    else return()
