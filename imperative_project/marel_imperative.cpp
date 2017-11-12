#include <iostream>
#include <string>
using namespace std;

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
  Piece pieces[3];
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

bool check_coordinate(string cell) {
  Coordinate coordinate = cell_to_coord(cell);
  if (coordinate.row < 0 || coordinate.row > 2
    || coordinate.column < 0 || coordinate.column > 2) {
    return false;
  } else if (marel_board[coordinate.row][coordinate.column] != '_') {
    return false;
  }

  return true;
}

void place_piece(char piece, string cell) {
  Coordinate coordinate = cell_to_coord(cell);
  if (check_coordinate(cell)) {
    marel_board[coordinate.row][coordinate.column] = piece;
  }
}

bool check_victory(char platform[3][3], char player) {
  int positions_pieces[3][2] = {{-1, -1}, {-1, -1}, {-1, -1}};

  for (int i = 0; i < 3; i++) {
    int index_piece = 0;
    int cord_x = 0;
    int cord_y = 1;

    for (int j = 0; j < 3; j++) {
      char piece = platform[i][j];

      if (piece == player) {
        positions_pieces[index_piece][cord_x] = i;
        positions_pieces[index_piece][cord_y] = j;
        index_piece++;
      }
    }
  }

  return false;
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
  cin >> name;
  cin >> piece_shape;

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

int main() {

  Player human_player = get_human_player();
  Player computer_player = get_computer_player(human_player.pieces[0].shape);

  snapshot_board(marel_board);

  place_piece('X', "A8"); // rejected
  place_piece('O', "B2"); // accepted
  place_piece('X', "C3"); // accepted
  place_piece('O', "C3"); // rejected

  snapshot_board(marel_board);

  // moves the piece to a position below if the piece shape of the human player is O
  char current_player_piece_shape = human_player.pieces[0].shape;

  move_piece(current_player_piece_shape, "B2", "B3");
  snapshot_board(marel_board);


  cout << is_valid_cell("B4") << endl;             // expect 0
  cout << is_valid_cell("C1") << endl;             // expect 1
  cout << cell_to_coord("A3").row << endl;         // expect 2
  cout << cell_to_coord("A3").column << endl;      // expect 0
  cout << cell_to_coord("금요일밤").row << endl;    // expect -1
  cout << cell_to_coord("금요일밤").column << endl; // expect -1

  return 0;
}
