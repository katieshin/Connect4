package net.connect4;

/**
 * A class for representing the state of a connect 4 game
 * 
 * @author anvil777
 *
 */
public final class Board {

	private char[][] board;

	public Board(int width, int height){
		board = new char[width][height];
		for(int i=0; i<width; i++){
			for(int j=0; j<height; j++){
				board[i][j] = ' ';
			}
		}
	}		
	
	public int width(){
		return board.length;
	}
	
	public int height(){
		return board[0].length;
	}
	
	/**
	 * Adds a piece to the top of a column if it isn't full,
	 * and returns true if successful.
	 * 
	 * @param column : the column to add a piece to
	 * @param piece : the token char of the piece to add
	 * @return true if successful and false if the column was already full.
	 */
	public boolean addPiece(int column, char piece){
		//find the top of the stack in the given column
		for(int j=0; j<height(); j++){
			if(board[column][j]==' '){
				board[column][j] = piece;
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Determines if a token has 4 in a row and returns the winner
	 * 
	 * @return the character of the token of the winner or null if there isn't one.
	 */
	public Character getWinner(){
		for(int i=0; i<width()-4; i++){
			for(int j=0; j<height(); j++){
				if(board[i][j]==' ') continue;
				char test = board[i][j];
				int count = 0;
				
				//check vertically
				for(int k=j; k<j+4 && k<height(); k++){
					if(board[i][k]==test) count++;
				}
				
				if(count>=4) return test;
				count = 0;
				
				//check horizontally
				for(int k=i; k<i+4 && k<width(); k++){
					if(board[k][j]==test) count++;
				}
				
				if(count>=4) return test;
				count = 0;
				
				//check up-diagonally
				for(int k=0; k<4 && i+k<width() && j+k<height(); k++){
					if(board[i+k][j+k]==test) count++;
				}
				
				if(count>=4) return test;
				count = 0;
				
				//check up-diagonally
				for(int k=0; k<4 && i+k<width() && j-k>=0; k++){
					if(board[i+k][j-k]==test) count++;
				}
				
				if(count>=4) return test;
				
			}
		}
		
		return null;
	}
	
	/**
	 * @return true if the board is currently full
	 */
	public boolean full(){
		for(int i=0; i<width(); i++){
			for(int j=0; j<height(); j++){
				if(board[i][j]==' ') return false;
			}
		}
		return true;
	}
	
	/**
	 * Converts this board to a string representation of a prolog list representation
	 * of this board with each sub-list being a column.
	 */
	public String toPrologList(){
		String s = "[";
		for(int i=0; i<board.length; i++){
			s+="[";
			for(int j=0; j<board[i].length; j++){
				s+= board[i][j]==' ' ? "0" : "\""+board[i][j]+"\"";
				if(j<board[i].length-1) s+=", ";
			}
			s+="]";
			if(i<board.length-1) s+=", ";
		}
		s+="]";
		return s;
	}
	
	@Override
	public String toString(){
		String s = "";
		for(int j=height()-1; j>=0; j--) {
			s+="|";
			for(int i=0; i<width(); i++) {
				s+=board[i][j]+"|";
			}
			if(j>0) s+="\n";
		}
		return s;
	}

}
