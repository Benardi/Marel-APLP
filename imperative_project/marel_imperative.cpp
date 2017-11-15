#include <iostream>
#include <string>
#include <stdlib.h>
#include <ctime>

using namespace std;

const int number_pieces_player = 3;

char marel_board [3][3] = { {'_','_','_'}, {'_','_','_'}, {'_','_','_'} };
char column_index [3] = {'A', 'B', 'C'};

struct Coordinate {
  int row;
  int column;
};

struct Piece {
  char shape;
  Coordinate coordinate;
};

struct Player {
  string name;
  Piece pieces[number_pieces_player];
};

Player create_player(string name, char piece_shape) {
  struct Player player;

  struct Piece piece1, piece2, piece3;

  struct Coordinate coord1, coord2, coord3;

  coord1.row = -1;
  coord1.column = -1;
  coord2.row = -1;
  coord2.column = -1;
  coord3.row = -1;
  coord3.column = -1;

  piece1.shape = piece_shape;
  piece1.coordinate = coord1;

  piece2.shape = piece_shape;
  piece2.coordinate = coord2;

  piece3.shape = piece_shape;
  piece3.coordinate = coord3;

  player.name = name;
  player.pieces[0] = piece1;
  player.pieces[1] = piece2;
  player.pieces[2] = piece3;

  return player;
}

inline void sleep() {
  clock_t start_time = clock();
  clock_t end_time = 1000000 + start_time;

  while(clock() != end_time);
}

bool is_valid_cell(string cell){
  if(cell.size() != 2){
    return false;
  }else{
    char column = toupper(cell.at(0));
    char row = cell.at(1);
    if(column != 'A' && column != 'B' && column != 'C'){
      return false;
    }else if(row != '1' && row != '2' && row != '3'){
      return false;
    }else{
      return true;
    }
  }
}

Coordinate cell_to_coord(string cell){
  struct Coordinate mapped;

  if(is_valid_cell(cell)){
    int column = toupper(cell.at(0)) - 65;
    int row = cell.at(1) - 49;

    mapped.column = column;
    mapped.row = row;

  }else{
    mapped.column =  -1 ;
    mapped.row =  -1 ;
  }

  return mapped;
}

bool is_valid_coordante(struct Coordinate coordinate) {
  return !(coordinate.row < 0) && !(coordinate.row > 2)
    && !(coordinate.column < 0) && !(coordinate.column > 2);
}

bool check_coordinate(string cell) {
  Coordinate coordinate = cell_to_coord(cell);

  bool is_valid_coordinate = is_valid_coordante(coordinate);
  bool is_empty_position =  marel_board[coordinate.row][coordinate.column] == '_';
  return is_valid_coordante && is_empty_position;
}

bool check_sequence(struct Piece pieces[3], bool check_column) {
  for (int i = 0; i < 2; i ++) {
    struct Coordinate coordinate_compare = pieces[i].coordinate;

    for (int j = i + 1; j < 3; j++) {
      struct Coordinate coordinate = pieces[j].coordinate;

      if (check_column && coordinate.column == coordinate_compare.column) {
        return false;
      } else if (!check_column && coordinate.row == coordinate_compare.row) {
        return false;
      }
    }
  }

  return true;
}

bool check_equals_coordinate(struct Piece pieces[3], bool check_column) {
  struct Coordinate piece_coordinate = pieces[0].coordinate;

  for (int i = 0; i < 3; i++) {
    struct Coordinate coordinate = pieces[i].coordinate;

    if (check_column && coordinate.column != piece_coordinate.column) {
      return false;
    } else if (!check_column && coordinate.row != piece_coordinate.row) {
      return false;
    }
  }

  return true;
}

bool check_diagonals(struct Piece pieces[3]) {
  for (int i = 0; i < 3; i++) {
    struct Coordinate coordinate = pieces[i].coordinate;
    bool main_diagonal = coordinate.row == coordinate.column;

    bool secondary_diagonal = coordinate.row == 2 && coordinate.column == 0 
                            || coordinate.row == 0 && coordinate.column == 2
                            || coordinate.row == 1 && coordinate.column == 1;
    
    if (!main_diagonal && !secondary_diagonal) {
      return false;
    }
  }

  return true;
}

bool check_victory(struct Piece pieces[3]) {
  for (int i = 0; i < 3; i++) {
    struct Coordinate coordinate = pieces[i].coordinate;

    if (!is_valid_coordante(coordinate)) {
      return false;
    }
  }

  bool is_sequence_column = check_sequence(pieces, true);
  bool is_sequence_row = check_sequence(pieces, false);
  bool is_equals_coordinate_column = check_equals_coordinate(pieces, true);
  bool is_equals_coordinate_row = check_equals_coordinate(pieces, false);

  bool victory_column = is_sequence_column && is_equals_coordinate_row;
  bool victory_row = is_sequence_row && is_equals_coordinate_column;
  bool victory_diagonal = is_sequence_row && is_sequence_column && check_diagonals(pieces);

  bool victory = victory_column || victory_row || victory_diagonal;
  return victory;
}

void place_piece(char piece, string cell) {
  Coordinate coordinate = cell_to_coord(cell);
  if (check_coordinate(cell)) {
    marel_board[coordinate.row][coordinate.column] = piece;
  }
}

bool check_move(char current_player_piece_shape, string current_cell, string final_cell) {

  Coordinate current_coord = cell_to_coord(current_cell);
  Coordinate final_coord = cell_to_coord(final_cell);

  char piece_shape = marel_board[current_coord.row][current_coord.column];
  if (current_coord.row < 0 || current_coord.row > 2
    || current_coord.column < 0 || current_coord.column > 2) {
    return false;
  } else if (piece_shape == '_') {
    return false;
  } else if (current_player_piece_shape != piece_shape) {
    return false;
  } else if (! check_coordinate(final_cell)) {
    return false;
  }

  return true;
}

void snapshot_board(char platform [3][3]) {
  endl(cout);
  cout << ' ' << ' ';

  for(int i = 0; i < 3; i++){
    cout << ' ' << column_index[i];
  }

  endl(cout);

  for(int i = 0; i < 3; i++){
    cout << ' ' << i+1 << ' ';
    for(int j = 0; j < 3; j++){
      cout << platform[i][j] << ' ';
    }

    endl(cout);
  }

  endl(cout);
}

bool move_piece(char current_player_piece_shape, string current_cell, string final_cell) {
    Coordinate current_coord = cell_to_coord(current_cell);
    Coordinate final_coord = cell_to_coord(final_cell);
    if (check_move(current_player_piece_shape, current_cell, final_cell)) {
      marel_board[final_coord.row][final_coord.column] = marel_board[current_coord.row][current_coord.column];
      marel_board[current_coord.row][current_coord.column] = '_';
      return true;
  } else {
      return false;
  }
}

Player get_human_player() {
  string name;
  char piece_shape;
  cout << "Choose the name of your player: ";
  cin >> name;
  cout << "Choose the shape of your piece: ";
  cin >> piece_shape;
  cout << endl <<" -- Welcome to the game " + name << " -- " << endl << endl;

  return create_player(name, piece_shape);
}

Player get_computer_player(char human_piece_shape) {
  string name = "Computer";
  char piece_shape = 'X';

  if (human_piece_shape == 'X') {
    piece_shape = 'O';
  }

  return create_player(name, piece_shape);
}

// get coordinate for the computer to place your piece
Coordinate get_coordinate_to_place_piece() {
  int random_row = rand() % 3;
  int random_column = rand() % 3;

  Coordinate coordinate;

  while (marel_board[random_row][random_column] != '_') {
    random_row = rand() % 3;
    random_column = rand() % 3;
  }

  coordinate.row = random_row;
  coordinate.column = random_column;

  return coordinate;
}

// get the position of the piece to be moved by a player
int get_position_piece(Player player, string current_cell) {
	Coordinate current_coord = cell_to_coord(current_cell);
	
	for (int i = 0; i < number_pieces_player; i++) {
		if (player.pieces[i].coordinate.row == current_coord.row && player.pieces[i].coordinate.column == current_coord.column) {
			return i;
		}
	}
	
	return -1;
}

void search_piece_computer(char computer_shape){

  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      if(marel_board[i][j] == computer_shape){
        marel_board[i][j] = '_';
        return;
      }
    }
  }
}


bool is_free(int x, int y){

  if(marel_board[x][y] == '_'){
    return true;
  }

  return false;
  
}

bool swap_piece(int x, int y, int new_x, int new_y, int shape){


  marel_board[x][y] = '_';   
  marel_board[new_x][new_y] = shape;    


}


bool is_position_valid(int x, int y) {

  if(x > -1 && x < 3 && y < 3 && y > -1){
    return true;
  }else{
    return false;
  }

  
}


void computer_move_pieces(char computer_shape) {

  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {

      if(marel_board[i][j] == computer_shape){

        if(i != 1 && j != 1 && is_free(1, 1)){
          swap_piece(i, j, 1, 1, computer_shape);
          return;
        }

        if(is_position_valid(i+1, j)){

          if(is_free(i+1, j)){
            swap_piece(i, j, i+1, j, computer_shape);
            return;
          }

        }

        if(is_position_valid(i-1, j)){

          if(is_free(i-1, j)){
            swap_piece(i, j, i-1, j, computer_shape);
            return;
          }

        }

        if(is_position_valid(i, j-1)){

          if(is_free(i, j-1)){
            swap_piece(i, j, i, j-1, computer_shape);
            return;
          }

        }

        if(is_position_valid(i, j+1)){

          if(is_free(i, j+1)){
            swap_piece(i, j, i, j+1, computer_shape);
            return;
          }

        }
                
      }
    }
  }

  snapshot_board(marel_board);
}


void move_pieces(bool has_computer_player, bool is_victory, Player player_one, Player player_two) {
	while(!is_victory) {
		snapshot_board(marel_board);
		
		int position_piece_in_player_one;
		string current_coordinate_player_one;
		string final_coordinate_player_one;
		bool is_not_played_valid_one = true;

		while(is_not_played_valid_one) {
			
			cout << "Player " + player_one.name << " choose the piece you are moving (according to the coordinates above on the map): ";
			cin >> current_coordinate_player_one; 
			
			cout << "Player " + player_one.name << " choose where you move your piece (according to the coordinates above on the map): ";
			cin >> final_coordinate_player_one;
			
			position_piece_in_player_one = get_position_piece(player_one, current_coordinate_player_one);
			
			is_not_played_valid_one = (position_piece_in_player_one == -1) || !check_move(player_one.pieces[position_piece_in_player_one].shape, current_coordinate_player_one, final_coordinate_player_one);
						
			if(is_not_played_valid_one) {
				cout << "Invalid player movement, please try again " + player_one.name << endl << endl;
			}
		}
		
		move_piece(player_one.pieces[position_piece_in_player_one].shape, current_coordinate_player_one, final_coordinate_player_one);
		player_one.pieces[position_piece_in_player_one].coordinate = cell_to_coord(final_coordinate_player_one);
		
		is_victory = check_victory(player_one.pieces);
		
		if (!is_victory) {
			snapshot_board(marel_board);

			if (!has_computer_player) {
				string current_coordinate_player_two;
				string final_coordinate_player_two;
				int position_piece_in_player_two;
				bool is_not_played_valid_two = true;

				while(is_not_played_valid_two) {
					
					cout << "Player " + player_two.name << " choose the piece you are moving (according to the coordinates above on the map): ";
					cin >> current_coordinate_player_two; 
			
					cout << "Player " + player_two.name << " choose where you move your piece (according to the coordinates above on the map): ";
					cin >> final_coordinate_player_two;
					
					position_piece_in_player_two = get_position_piece(player_two, current_coordinate_player_two);
			
					is_not_played_valid_two = (position_piece_in_player_two == -1) || !check_move(player_two.pieces[position_piece_in_player_two].shape, current_coordinate_player_two, final_coordinate_player_two);
				
					if(is_not_played_valid_two) {
						cout << "Invalid player movement, please try again " + player_two.name << endl << endl;
					}
				}

				move_piece(player_two.pieces[position_piece_in_player_two].shape, current_coordinate_player_two, final_coordinate_player_two);
				player_two.pieces[position_piece_in_player_two].coordinate = cell_to_coord(final_coordinate_player_two);
				
			} else {
        sleep();

        cout << player_two.name + " moved your piece on the board:" << endl;
        computer_move_pieces(player_two.pieces[0].shape);
			}
			
			is_victory = check_victory(player_two.pieces);
		}		
	}
	
	Player player_win = (check_victory(player_one.pieces)) ? player_one : player_two;

  snapshot_board(marel_board);
  cout << "Congratulations, player " + player_win.name << ", you won." << endl;
	cout << "End Game!" << endl;
}



void place_pieces(bool has_computer_player) {
	cout << " -- PLAYER ONE -- " << endl;
	Player player_one = get_human_player();
	Player player_two;

	if (!has_computer_player) {
		cout << " -- PLAYER TWO -- " << endl;
		player_two = get_human_player();
	} else {
		player_two = get_computer_player(player_one.pieces[0].shape);
	}

	bool is_victory = false;
	int count = 0;

	while (count < number_pieces_player) {
		snapshot_board(marel_board);
		string coordinate_player_one;
		string coordinate_player_two;
		bool is_not_played_valid_one = true;

		while(is_not_played_valid_one) {
			cout << "Player " + player_one.name << " choose where you place your piece (according to the coordinates above on the map): ";

			cin >> coordinate_player_one;

			is_not_played_valid_one = !check_coordinate(coordinate_player_one);

			if(is_not_played_valid_one) {
				cout << "Invalid player movement, please try again " + player_one.name << endl << endl;
			}
		}

		player_one.pieces[count].coordinate = cell_to_coord(coordinate_player_one);
		place_piece(player_one.pieces[count].shape, coordinate_player_one);

		// You can only win if you have three pieces and only one player has three pieces in that state
    is_victory = (count == 2) && check_victory(player_one.pieces);

		if (!is_victory) {
      snapshot_board(marel_board);

			if (!has_computer_player) {
				bool is_not_played_valid_two = true;

				while(is_not_played_valid_two) {
					cout << "Player " + player_two.name << " choose where you place your piece (according to the coordinates above on the map): ";

					cin >> coordinate_player_two;

					is_not_played_valid_two = !check_coordinate(coordinate_player_two);

					if(is_not_played_valid_two) {
						cout << "Invalid player movement, please try again " + player_two.name << endl << endl;
					}
				}

				player_two.pieces[count].coordinate = cell_to_coord(coordinate_player_two);
				place_piece(player_two.pieces[count].shape, coordinate_player_two);
			} else {
        sleep();
        cout << player_two.name + " place your piece on the board:" << endl;

        Coordinate computer_coordinate = get_coordinate_to_place_piece();

        player_two.pieces[count].coordinate.row = computer_coordinate.row;
        player_two.pieces[count].coordinate.column = computer_coordinate.column;
        marel_board[computer_coordinate.row][computer_coordinate.column] = player_two.pieces[count].shape;
			}
			
			// Just check the second player because the first one has already been verified
			is_victory = (count == 2) && check_victory(player_two.pieces);
		}

		count++;
	}

	move_pieces(has_computer_player, is_victory, player_one, player_two);
}

void main_menu() {
	cout << "###############################################" << endl;
	cout << "#          MAREL THE BEST GAME EVER           #" << endl;
	cout << "#          Play and have lots of fun          #" << endl;
	cout << "###############################################" << endl << endl;

	cout << "Choose an option:" << endl;
	cout << "Option (1): play with a friend." << endl;
	cout << "Option (2): play with the computer." << endl;
	cout << "Option (anything): quit the game." << endl << endl;
	int opcao;

  cout << "Option: ";
	cin >>  opcao;
  cout << endl;

	if (opcao == 1) {
		place_pieces(false);
	} else if (opcao == 2) {
		place_pieces(true);
	} else {
		cout << "Bye! See you later." << endl;
	}
}

int main() {
  main_menu();

  return 0;
}
