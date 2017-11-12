#include <iostream>
#include <string>
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
  int sequence = 0;

  for (int i = 0; i < 3; i ++) {
    for (int j = 0; j < 3; j++) {
      struct Coordinate coordinate = pieces[j].coordinate;

      if (check_column && coordinate.column == sequence) {
        sequence++;
      } else if (!check_column && coordinate.row == sequence) {
        sequence++;
      }
    }
  }

  bool is_sequence = sequence == 3;
  return is_sequence;
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
  bool victory_diagonal = is_sequence_row && is_sequence_column;

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
  cout << "Choose the name of your player:" << endl;
  cin >> name;
  cout << "Choose the shape of your piece:" << endl;
  cin >> piece_shape;
  cout << " -- Welcome to the game " + name << " -- " << endl << endl;		
  
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

void move_pieces(bool has_computer_player, bool is_victory) {
	while(!is_victory) {
		snapshot_board(marel_board);
		// to do
		is_victory = true; //remove later
	}	
	
	cout << "end game" << endl;
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
			cout << "Player " + player_one.name << " Choose where you place your piece (according to the coordinates above on the map):" << endl;
		
			cin >> coordinate_player_one;
			
			is_not_played_valid_one = !check_coordinate(coordinate_player_one);
			
			if(is_not_played_valid_one) {
				cout << "Invalid player movement, please try again " + player_one.name << endl << endl;
			}
		}
		
		player_one.pieces[count].coordinate = cell_to_coord(coordinate_player_one);
		place_piece(player_one.pieces[count].shape, coordinate_player_one);
    cout << "Vitoria ou nao" << endl;
    cout << check_victory(player_one.pieces) << endl;
    cout << "fim" << endl;
		
		if(count == 2) {
			is_victory =  false; // Put the function that verifies the victory in place of the false
		}
		
		if (!is_victory) {
			if (!has_computer_player) {
				snapshot_board(marel_board);
				bool is_not_played_valid_two = true;	
				
				while(is_not_played_valid_two) {
					cout << "Player " + player_two.name << " Choose where you place your piece (according to the coordinates above on the map):" << endl;
					
					cin >> coordinate_player_two;
				
					is_not_played_valid_two = !check_coordinate(coordinate_player_two);
				
					if(is_not_played_valid_two) {
						cout << "Invalid player movement, please try again " + player_two.name << endl << endl;
					}
				}
					
				player_two.pieces[count].coordinate = cell_to_coord(coordinate_player_two);
				place_piece(player_two.pieces[count].shape, coordinate_player_two);
					
			} else {
				// to do computer player
			}
		}
		
		count++;
	}
	
	move_pieces(has_computer_player, is_victory);
}

void menu_principal() {
	cout << "###############################################" << endl;
	cout << "#          MAREL THE BEST GAME EVER           #" << endl;
	cout << "#          Play and have lots of fun          #" << endl;
	cout << "###############################################" << endl << endl;
	
	cout << "Choose an option:"<< endl;
	cout << "Option 1: play with a friend" << endl;
	cout << "Option 2: play with the computer" << endl;
	cout << "Option anything: quit the game" << endl;
	int opcao;
	cin >>  opcao;
	
	if (opcao == 1) {
		place_pieces(false);		
	} else if (opcao == 2) {
		place_pieces(true);
	} else {
		cout << "Bye see you later" << endl;
	}
}

int main() {

//  Player human_player = get_human_player();
//  Player computer_player = get_computer_player(human_player.pieces[0].shape);

//  snapshot_board(marel_board);
  
  menu_principal();

//  place_piece('X', "A8"); // rejected
//  place_piece('O', "B2"); // accepted
//  place_piece('X', "C3"); // accepted
//  place_piece('O', "C3"); // rejected

//  snapshot_board(marel_board);

  // moves the piece to a position below if the piece shape of the human player is O
//  char current_player_piece_shape = human_player.pieces[0].shape;

//  move_piece(current_player_piece_shape, "B2", "B3");
//  snapshot_board(marel_board);


//  cout << is_valid_cell("B4") << endl;             // expect 0
//  cout << is_valid_cell("C1") << endl;             // expect 1
//  cout << cell_to_coord("A3").row << endl;         // expect 2
//  cout << cell_to_coord("A3").column << endl;      // expect 0
//  cout << cell_to_coord("금요일밤").row << endl;    // expect -1
//  cout << cell_to_coord("금요일밤").column << endl; // expect -1

  return 0;
}
