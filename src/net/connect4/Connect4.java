package net.connect4;

import java.util.Scanner;

public class Connect4 {

	private Connect4(){}

	
	
	public static void main(String[] args){
		
		Board b = new Board(6, 7);
		
		Scanner input = new Scanner(System.in);
		
		a:while(true){
			System.out.println(b+"\n");
			System.out.print("Enter Next Column (or q to quit):");

			int column = 0;
			while(true){
				try{
					String cmd = input.next();
					
					if(cmd.equalsIgnoreCase("q")) break a;
					
					column = Integer.parseInt(cmd);
					b.addPiece(column, '*');
				}catch(NumberFormatException | ArrayIndexOutOfBoundsException e){
					System.out.print("Sorry, that is not a valid input:");
					continue;
				}
				break;
			}
		}
		
		input.close();
		
	}
	
	public static class Board {
		
		private char[][] board;

		private Board(int width, int height){
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
		
		public void addPiece(int column, char piece){
			//find the top of the stack in the given column
			for(int j=0; j<height(); j++){
				if(board[column][j]==' '){
					board[column][j] = piece;
					return;
				}
			}
			
			throw new ArrayIndexOutOfBoundsException("Sorry, that row is full");
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
	
	
	
	
	
}
