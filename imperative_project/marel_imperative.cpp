#include <iostream>
using namespace std;

char marel_board [3][3] = { {'_','_','_'}, {'_','_','_'}, {'_','_','_'} };
char column_index [3] = {'A', 'B', 'C'};

/*
char map_cell(int content){
  if(content == 1){
    return 'X';
  }
  else if (content == 2){
    return 'O';
  }
  else {
   return '_';
  }

}*/

bool place_piece(char piece, int row, int column){
  if(row < 0 || row > 2 || column < 0 || column > 2 ){
    return false;
  }else if(marel_board[row][column] != '_'){
    return false;
  }
  else{
    marel_board[row][column] = piece;
    return true;
  }

}

void snapshot_board(char platform [3][3]){

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

bool move_piece(int current_row, int current_column, int new_row, int new_column){
	// TODO chamar o metodo de lucas para validar o movimento
  if(current_row < 0 || current_row > 2 || current_column < 0 || current_column > 2 || new_row < 0 || new_row > 2 || new_column < 0 || new_column > 2){
    return false;
  }else if(marel_board[current_row][current_column] == '_'){
    return false;
  }else{
    marel_board[new_row][new_column] = marel_board[current_row][current_column];
    marel_board[current_row][current_column] = '_';
    return true;
  }

}

int main()
{

  snapshot_board(marel_board);
  cout << place_piece('X', 1, -1) << endl;
  cout << place_piece('O', 1, 1) << endl;
  cout << place_piece('X', 1, 1) << endl;
  snapshot_board(marel_board);

// movimenta a peça para uma casa acima
  cout << move_piece(1,1,0,1) << endl;
  snapshot_board(marel_board);


  return 0;
}
