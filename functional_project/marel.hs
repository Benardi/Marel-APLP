check_diagonal :: Char -> [[Char]] -> Bool
check_diagonal shape marel_board = do
  if(((marel_board !! 0) !! 0 == shape) && ((marel_board !! 1) !! 1 == shape) && ((marel_board !! 2) !! 2 == shape))
    then True
  else False

main :: IO ()
main = do
    shape <- getChar
    let marel_board  = [['X','_','_'],['_','X','_'],['_','_','X']]
    print (check_diagonal shape marel_board)
