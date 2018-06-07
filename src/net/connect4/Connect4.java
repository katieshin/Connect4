package net.connect4;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Scanner;

import gnu.prolog.vm.PrologException;

public class Connect4 {

	private Connect4(){}

	
	
	public static void main(String[] args) throws PrologException, IOException, InterruptedException{
		
		Board b = new Board(6, 7);
		
//		Scanner input = new Scanner(System.in);
		
		final Process swi = new ProcessBuilder("swipl", "-s", "src/net/connect4/Connect4Logic.pl").start();
		
		swi.getInputStream();
		
		new Thread(() -> {
            BufferedReader ir = new BufferedReader(new InputStreamReader(swi.getInputStream()));
            String line = null;
            try {
                while(true){
                	line = ir.readLine();
                    if(line!=null) System.out.printf("%s\n", line);
                }
            } catch(IOException e) {}
        }).start();
		
		final Scanner sc = new Scanner(System.in);
		final BufferedWriter bf = new BufferedWriter(new OutputStreamWriter(swi.getOutputStream()));
		final String newLine = System.getProperty("line.separator");
		
		sc.nextLine();
		
		System.out.println("HI");

		Thread.sleep(1000);

		System.out.println("HI");
		
		bf.write("addPiece([[0,0,0],[0,0,0],[0,0,0]], 2, a, B).\n");
		bf.newLine();
		bf.flush();
		
		Thread.sleep(1000);

		System.out.println("HI");
		
		bf.write("halt.\n");
		bf.newLine();
		bf.flush();
		
		/*final Scanner sc = new Scanner(System.in);
		final BufferedWriter bf = new BufferedWriter(new OutputStreamWriter(swi.getOutputStream()));
		final String newLine = System.getProperty("line.separator");
		while(true){
			String c = sc.nextLine();
			bf.write(c);
			bf.newLine();
			bf.flush();
		}*/
		
		/*BufferedReader out = new BufferedReader(new InputStreamReader(swi.getInputStream()));
		
		String line;
		while(swi.isAlive()){
			line = out.readLine();
			if(line!=null) System.out.print(line);
		}*/
		
		/*a:while(true){
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
		
		input.close();*/
		
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
