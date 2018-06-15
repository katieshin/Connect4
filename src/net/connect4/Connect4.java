package net.connect4;

import java.io.IOException;
import java.util.Scanner;

public class Connect4 {

	//it's a little of a kludge, but it works
	public static String SWIPATH;
	
	//So you can't instance the Connect4 class
	private Connect4(){}
	
	
	public static void main(String[] args) throws IOException, InterruptedException{
		
		new Solver(7, 6, 'a', 'b');
		
		System.exit(0);
		
		//get the path to the swi-prolog executable
		if(args.length>0){
			SWIPATH = args[0];
			if(SWIPATH.charAt(0)=='"'){
				SWIPATH = SWIPATH.substring(1, SWIPATH.length()-2);
			}
		}else {
			SWIPATH = "swipl";
		}
		
		//Stores the game's instance
		Board board = new Board(5, 6, 3);
		
		Player[] players = new Player[2];
		
		//set up the game
		Scanner input = new Scanner(System.in);
		for(int i=0, numAI=0; i<players.length; i++){
			System.out.print("Is player #"+(i+1)+" a human or AI?");
			String response;
			while(true){
				response = input.nextLine();
				if(response.equalsIgnoreCase("human") || response.equalsIgnoreCase("h")){
					System.out.print("Please enter your name:");
					String name = input.nextLine();
					players[i] = new Player.HumanPlayer(name==null || name.equals("") ? "player #"+(i+1) : name, i==0?'a':i==1?'b':(""+i).charAt(0));
					break;
				}else if(response.equalsIgnoreCase("AI")){
					String name = numAI==0 ? "God-Like AI of DOOM" : numAI==1 ? "God-Like AI of DISPAIR" : "God-Like AI #"+(numAI+1);
					players[i] = new Player.GodlikePrologAIOfDoom(name, i==0?'a':i==1?'b':(""+i).charAt(0));
					numAI++;
					break;
				}else{
					System.out.print("Sorry, did not understand your response. Input \"human\" for a human player or \"AI\" for an AI:");
				}
			}
		}

		//loop through each turn
		int currentPlayer = 0;		
		while(board.getWinner()==null && !board.full()){
			System.out.println(board);
			int nextColumn = players[currentPlayer].pickColumnToPlay(board);
			
			if(nextColumn==-1){
				System.out.println("Quitting");
				currentPlayer=-1;
				break;
			}else if(nextColumn<0 || nextColumn>=board.width()){
				//out of range
				System.out.println("Sorry, that column is out of range. Please enter a different column:");
			}else{
				if(!board.addPiece(nextColumn, players[currentPlayer].token)){
					System.out.println("Sorry, that column is full. Please enter a different column:");
				}else{
					currentPlayer++;
					currentPlayer%=players.length;
				}
			}
		}

		
		
		System.out.println(board);
		
		if(currentPlayer==-1){
			
		}else if(board.full()){
			System.out.println("\nCongrats!");
			System.out.println("Your long fought and gruelling battle ended in a tie!");
		}else{
			System.out.println("\nCongrats!");
			System.out.println(players[(currentPlayer-1+players.length)%players.length].name+" won!");
		}		

		input.close();
		for(int i=0; i<players.length; i++){
			players[i].destroy();
		}
		System.exit(0);
		
	}
	
	
}
