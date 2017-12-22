import Data.Map
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

coord_to_cell :: Coordinate -> String
coord_to_cell coordinate
    | row coordinate == 0 && column coordinate == 0 = "A1"
    | row coordinate == 1 && column coordinate == 0 = "A2"
    | row coordinate == 2 && column coordinate == 0 = "A3"
    | row coordinate == 0 && column coordinate == 1 = "B1"
    | row coordinate == 1 && column coordinate == 1 = "B2"
    | row coordinate == 2 && column coordinate == 1 = "B3"
    | row coordinate == 0 && column coordinate == 2 = "C1"
    | row coordinate == 1 && column coordinate == 2 = "C2"
    | row coordinate == 2 && column coordinate == 2 = "C3"
    | otherwise = "XX"    

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
    putStrLn("")
    putStr("Option is: ")
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

get_shape_computer :: Char -> Char
get_shape_computer shape_player = if (shape_player == 'X') || (shape_player == 'x') then 'O' else 'X'  

is_adjacent_move :: String -> String -> Bool
is_adjacent_move coord_from coord_to
    | (row (cell_to_coord coord_to)) == -1 || (column (cell_to_coord coord_to)) == -1 = False
    | (abs((row (cell_to_coord coord_from)) - (row (cell_to_coord coord_to))) == 1) && ((column (cell_to_coord coord_from)) - (column (cell_to_coord coord_to)) == 0) = True
    | (abs((column (cell_to_coord coord_from)) - (column (cell_to_coord coord_to))) == 1) && ((row (cell_to_coord coord_from)) - (row (cell_to_coord coord_to)) == 0) = True
    | (column (cell_to_coord coord_to) == 1) && (row (cell_to_coord coord_to) == 1) = True
    | (column (cell_to_coord coord_from) == 1) && (row (cell_to_coord coord_from) == 1) = True
    | otherwise = False

get_moves_computer :: [[Char]] -> [String] -> Map String [String] -> Map String [String]
get_moves_computer board [] possible_moves = possible_moves  
get_moves_computer board pieces possible_moves = do 
    get_moves_computer board (tail pieces) (Data.Map.insert (head pieces) [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (is_valid_moviment (head pieces) (coord_to_cell (Coordinate x y)) board)] possible_moves)

get_adjacent_pieces :: String -> String -> [String]
get_adjacent_pieces piece_from piece_to = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (coord_to_cell (Coordinate x y)) /= piece_to, (coord_to_cell (Coordinate x y)) /= piece_from, (is_adjacent_move piece_to (coord_to_cell (Coordinate x y)))]

get_possible_move_victory :: [[Char]] -> Char -> String -> [String] -> String
get_possible_move_victory board shape piece [] = "XX"
get_possible_move_victory board shape piece moves = do
    let board_after_move = (place_piece '_' piece (place_piece shape (head moves) board))
    if check_victory shape board_after_move then (head moves) else do
        get_possible_move_victory board shape piece (tail moves)

get_possible_victory :: [[Char]] -> Char -> Map String [String] -> [String] -> (String, String)
get_possible_victory board shape possible_moves [] = ("XX", "XX")
get_possible_victory board shape possible_moves pieces = do
    let possible_move_victory = (get_possible_move_victory board shape (head pieces) (possible_moves ! (head pieces)))
    if possible_move_victory == "XX" then get_possible_victory board shape possible_moves (tail pieces) else ((head pieces), possible_move_victory) 

get_possible_adjacent_piece :: [[Char]] -> Char -> String -> [String] -> String
get_possible_adjacent_piece board shape_computer piece [] = "XX"
get_possible_adjacent_piece board shape_computer piece moves = do
    let moves_adjacent = [y | y <- (get_adjacent_pieces piece (head moves)), (board !! (row (cell_to_coord y)) !! (column (cell_to_coord y))) == shape_computer]
    if moves_adjacent /= [] then (head moves) else do
        get_possible_adjacent_piece board shape_computer piece (tail moves)

get_possible_adjacent_pieces :: [[Char]] -> Char -> Map String [String] -> [String] -> (String, String)
get_possible_adjacent_pieces board shape_computer possible_moves [] = ("XX", "XX")
get_possible_adjacent_pieces board shape_computer possible_moves pieces = do
    let possible_adjacent_move = (get_possible_adjacent_piece board shape_computer (head pieces) (possible_moves ! (head pieces)))
    if possible_adjacent_move == "XX" then get_possible_adjacent_pieces board shape_computer possible_moves (tail pieces) else ((head pieces), possible_adjacent_move)

get_anything_move :: Map String [String] -> [String] -> (String, String)
get_anything_move possible_moves [] = ("XX", "XX")
get_anything_move possible_moves pieces = do
    let moves = (possible_moves ! (head pieces))
    if moves /= [] then ((head pieces), (head moves)) else get_anything_move possible_moves (tail pieces) 

-- Order of priority of movements:
-- 1. If movement is movement to win
-- 2. If the movement is a movement that prevents the player from winning
-- 3. If the movement leaves you close to your pieces
-- 4. Any possible movement
get_best_move_computer :: [[Char]] -> Char -> Char -> Map String [String] -> (String, String)
get_best_move_computer board shape_player shape_computer possible_moves = do
    let move_victory = get_possible_victory board shape_computer possible_moves (keys possible_moves)
    if move_victory == ("XX", "XX") then do
        let move_enemy_victory = get_possible_victory board shape_player possible_moves (keys possible_moves)
        if move_enemy_victory == ("XX", "XX") then do
            let move_adjacent_piece = get_possible_adjacent_pieces board shape_computer possible_moves (keys possible_moves)
            if move_adjacent_piece == ("XX", "XX") then do
                get_anything_move possible_moves (keys possible_moves)
            else move_adjacent_piece
        else move_enemy_victory
    else move_victory

check_possible_enemy_victory :: [[Char]] -> Char -> String -> Bool
check_possible_enemy_victory board shape_enemy piece = do
    let board_update = place_piece shape_enemy piece board
    if (check_victory shape_enemy board_update) then True else False

get_possible_enemy_victory :: [[Char]] -> Char -> String
get_possible_enemy_victory board shape_enemy = do
    let possible_piece_enemy_victory = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (board !! x !! y) == '_', check_possible_enemy_victory board shape_enemy (coord_to_cell (Coordinate x y))]
    if possible_piece_enemy_victory /= [] then (head possible_piece_enemy_victory) else "XX"

get_possible_place_victory :: [[Char]] -> Char -> String
get_possible_place_victory board shape = do
    let possible_place_victory = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (board !! x !! y) == '_', check_possible_enemy_victory board shape (coord_to_cell (Coordinate x y))]
    if possible_place_victory /= [] then head possible_place_victory
    else "XX"
    
get_possible_adjacent_piece_to_shape :: [[Char]] -> [String] -> String
get_possible_adjacent_piece_to_shape board [] = "XX"
get_possible_adjacent_piece_to_shape board pieces_shape = do
    let piece = (head pieces_shape)
    let adjacente_coordinates_piece = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (board !! x !! y) == '_', (coord_to_cell (Coordinate x y)) /= piece, (is_adjacent_move piece (coord_to_cell (Coordinate x y)))]
    let adjacente_coordinates_to_place = [y | y <- adjacente_coordinates_piece, (board !! (row (cell_to_coord y)) !! (column (cell_to_coord y))) == '_']
    if adjacente_coordinates_to_place /= [] then (head adjacente_coordinates_to_place) else get_possible_adjacent_piece_to_shape board (tail pieces_shape)

get_best_place_computer :: [[Char]] -> Char -> Char -> String
get_best_place_computer board shape_player shape_computer  = do
    let pieces_player = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (board !! x !! y) == shape_player]
    let pieces_computer = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (board !! x !! y) == shape_computer]
    let possible_place = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (board !! x !! y) == '_']
    
    let possible_piece_victory = get_possible_place_victory board shape_computer
    if possible_piece_victory == "XX" then do
        let possible_piece_enemy_victory = get_possible_enemy_victory board shape_player
        if possible_piece_enemy_victory == "XX" then do
            let possible_adjacent_place_to_player = get_possible_adjacent_piece_to_shape board pieces_player
            if possible_adjacent_place_to_player == "XX" then do
                let possible_adjacent_place_to_computer = get_possible_adjacent_piece_to_shape board pieces_computer
                if possible_adjacent_place_to_computer == "XX" then (head possible_place) else possible_adjacent_place_to_computer
            else possible_adjacent_place_to_player
        else possible_piece_enemy_victory
    else possible_piece_victory

receive_placement board = do
  coord <- getLine
  if is_valid_placement coord board
    then return coord
  else do
    putStr("\nPlease choose a valid coordinate for your placement: ")
    receive_placement board

receive_movement board shape = do
  org_cell <- getLine
  if is_valid_movement_orig org_cell shape board
    then return org_cell
  else do
    putStr("\nPlease choose a valid coordinate for your movement: ")
    receive_movement board shape

placementRound :: Int -> [[Char]] -> Player -> Player -> Char -> Char -> Bool -> IO [[Char]]
placementRound 0  board player1 player2 shape1 shape2 is_player_computer = return board
placementRound n board player1 player2 shape1 shape2 is_player_computer = do
  putStr("\n" ++ (playerName player1) ++ ", please choose a coordinate to place your cell: ")
  coord1 <- receive_placement board
  let board_plcm1 = (place_piece shape1 coord1 board)
  snapshot_board board_plcm1
  
  if not (check_victory shape1 board_plcm1) then do
    if is_player_computer then do
        putStrLn("\nMovement of the computer.")
        let coord_computer = get_best_place_computer board_plcm1 shape1 shape2
        let board_plcm2 = (place_piece shape2 coord_computer board_plcm1)
        snapshot_board board_plcm2
        placementRound (n-1) board_plcm2 player1 player2 shape1 shape2 is_player_computer
    else do
        putStr("\n" ++ (playerName player2) ++ ", please choose a coordinate to place your cell: ")
        coord2 <- receive_placement board_plcm1
        let board_plcm2 = (place_piece shape2 coord2 board_plcm1)
        snapshot_board board_plcm2
        placementRound (n-1) board_plcm2 player1 player2 shape1 shape2 is_player_computer
  else return board_plcm1 

movementRound :: [[Char]] -> Player -> Player -> Char -> Char -> Bool -> IO ()
movementRound board player1 player2 shape1 shape2 is_player_computer = do
  putStr("\n" ++ (playerName player1) ++ ", please choose a piece to be moved: ")
  coord1_from <- receive_movement board shape1
  putStr("\n" ++ (playerName player1) ++ ", please choose to where it should be moved: ")
  coord1_to <- getLine

  if is_valid_moviment coord1_from coord1_to board then do
    let board_mvm1 = (move_piece coord1_from coord1_to board)
    snapshot_board board_mvm1
    if check_victory shape1 board_mvm1
        then putStrLn("\n" ++ (playerName player1) ++ " has won!\n")
    else do
        if is_player_computer then movementRoundPlayerComputer board_mvm1 player1 player2 shape1 shape2
        else movementRoundPlayerTwo board_mvm1 player1 player2 shape1 shape2
  else do
    putStrLn("\nInvalid move for player one, please choose a valid movement.")
    movementRound board player1 player2 shape1 shape2 is_player_computer

movementRoundPlayerTwo board player1 player2 shape1 shape2 = do
  putStr("\n" ++ (playerName player2) ++ ", please choose a piece to be moved: ")
  coord2_from <- receive_movement board shape2
  putStr("\n" ++ (playerName player2) ++ ", please choose to where it should be moved: ")
  coord2_to <- getLine

  if is_valid_moviment coord2_from coord2_to board then do
    let board_mvm2 = (move_piece coord2_from coord2_to board)
    snapshot_board board_mvm2
    if check_victory shape2 board_mvm2
        then putStrLn("\n" ++ (playerName player2) ++ " has won!\n")
    else movementRound board_mvm2 player1 player2 shape1 shape2 False
  else do
    putStrLn("\nInvalid move for player two, please choose a valid movement.")
    movementRoundPlayerTwo board player1 player2 shape1 shape2

movementRoundPlayerComputer board player1 player_computer shape1 shape_computer = do
  putStrLn("\nMovement of the computer.")
  let pieces_computer = [coord_to_cell (Coordinate x y) | x <- [0..2], y <- [0..2], (board !! x !! y) == shape_computer]
  let possibles_moves = get_moves_computer board pieces_computer Data.Map.empty
  let best_move = get_best_move_computer board shape1 shape_computer possibles_moves
  let board_mvm2 = (move_piece (fst best_move) (snd best_move) board)
  snapshot_board board_mvm2
  if check_victory shape_computer board_mvm2
      then putStrLn("\nComputer has won!\n")
  else do
    putStrLn("\nThe computer player made his move.")
    movementRound board_mvm2 player1 player_computer shape1 shape_computer True

selectShape2 player2 shape1 = do
  shapeTwo <- getChar
  getLine -- cleans buffer

  if shapeTwo == shape1 then do
    putStr("\n" ++ (playerName player2) ++ ", please choose a different shape of " ++ [shape1] ++ ": ")
    selectShape2 player2 shape1
  else do
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

        putStr("\nPlayer one choose the name of your player: ")
        name <- getLine
        let player1 = Player name
        putStr("\n" ++ (playerName player1) ++ ", please choose the shape of your piece: ")
        shape1 <- getChar
        getLine -- cleans buffer
        
        if option == "1" then do
            putStr("\nPlayer two choose the name of your player: ")
            name <- getLine
            let player2 = Player name 
            putStr("\n" ++ (playerName player2) ++ ", please choose the shape of your piece: ")
            shape2 <- selectShape2 player2 shape1
            
            snapshot_board marel_board

            board_past_placement <- placementRound 3 marel_board player1 player2 shape1 shape2 False
            snapshot_board board_past_placement

            if check_victory shape1 board_past_placement
               then putStrLn((playerName player1) ++ " has won!\n")
            else do
               if check_victory shape2 board_past_placement
                   then putStrLn((playerName player2) ++ " has won!\n")
               else movementRound board_past_placement player1 player2 shape1 shape2 False
        else do 
            let player_computer = Player "Computer" 
            let shape_computer = get_shape_computer shape1
            snapshot_board marel_board
            
            board_past_placement <- placementRound 3 marel_board player1 player_computer shape1 shape_computer True

            if check_victory shape1 board_past_placement
               then putStrLn((playerName player1) ++ " has won!\n")
            else do
               if check_victory shape_computer board_past_placement
                   then putStrLn((playerName player_computer) ++ " has won!\n")
               else movementRound board_past_placement player1 player_computer shape1 shape_computer True
    else return()
