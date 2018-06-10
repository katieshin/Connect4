package net.connect4;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Scanner;
import java.util.Stack;

/**
 * An abstract class representing a player of a connect4 game.
 * A player could be a human player using the console, or a supreme
 * AI god, or even an AI randomly picking moves.
 *
 */
public abstract class Player {

	public final String name;
	public final char token;

	public Player(String name, char token){
		this.name = name;
		this.token = token;
	}

	/**
	 * Gets the column that this player wants to play in given the current state of the board.
	 * If the player would like to quit/forfeit, return -1.
	 * 
	 * @param gameState : the current state of the game board
	 * @return the next column for this player to play in or -1 if they forfeit
	 */
	public abstract int pickColumnToPlay(Board gameState);
	
	/**
	 * Releases all resources used by this player
	 */
	public abstract void destroy();
	
	/**
	 * A Human Connect4 player playing from the console.
	 *
	 */
	public static class HumanPlayer extends Player{

		private Scanner input;
		
		public HumanPlayer(String name, char token){
			super(name, token);
			input = new Scanner(System.in);
		}

		@Override
		public int pickColumnToPlay(Board gameState){
			System.out.print(name+", pick a column to play in (or 'q' to forfeit and quit):");
			
			int column;
			while(true){
				
				//get the player's input
				String move = input.nextLine();
				if(move.equals("q")){
					column = -1;
					break;
				}
				
				//try to parse
				try{
					column = Integer.parseInt(move);
					if(column<0 || column>=gameState.width()) {
						//out of range
						System.out.print("Sorry, that column is out of range. Please enter a different column:");
					}else{
						break;
					}
				}catch(NumberFormatException e){
					//bad entry
					System.out.print("Sorry, could not parse your next move. Please try again:");
				}				
				
			}
			
			return column;
		}

		@Override
		protected void finalize(){
			//'cause resource leaks are bad
			input.close();
		}

		@Override
		public void destroy(){
			finalize();
		}
		
	}
	
	/**
	 * A player object that uses a separate SWI-Prolog process loaded with an implementation
	 * of the MinMax algorithm in order to perfectly determine it's next move.
	 * 
	 * @author anvil777
	 *
	 */
	public static class GodlikePrologAIOfDoom extends Player {

		private static final Object LOCK = 0;
		
		private final Process swi;
		private final BufferedWriter swiInput;
		private final Thread swiOutputThread;
		private final Stack<String> swiOutput;
		
		private volatile boolean running;
		private volatile boolean thinking;
		
		public GodlikePrologAIOfDoom(String name, char token) throws IOException{
			super(name, token);
			running = true;
			swi = new ProcessBuilder("swipl", "-s", "src/net/connect4/Connect4Logic.pl").start();
			
			swiInput = new BufferedWriter(new OutputStreamWriter(swi.getOutputStream()));
			swiOutput = new Stack<String>();
			swiOutputThread = new Thread(() -> {
				//create an object to watch for output from swi
				BufferedReader ir = new BufferedReader(new InputStreamReader(swi.getInputStream()));
	            String line = null;
	            
	            try{
	            	//while there is output to be read
	            	while(swi.isAlive()){
	            		
	            		//get the next line of output
	            		line = ir.readLine();
	            		
	            		//if that output is meaningful (yes, the length 0 stuff is important)
	            		if(line!=null && line.trim().length()>0) {
	            			
	            			//add the output to the stack
	            			synchronized(swiOutput){
	            				swiOutput.push(line);
	            			}
	            			
	            			//unlock the main thread if it is waiting
	            			if(thinking){
	            				synchronized(LOCK){
	            					thinking = false;
	            					LOCK.notifyAll();
	            				}
	            			}
	            		}
	                }
	            }catch(IOException e){
	            	//something done borked
	            	e.printStackTrace();
	            }
			});
			swiOutputThread.start();
		}

		@Override
		public int pickColumnToPlay(Board gameState){
			//query swi to ask which column the AI should play in in order to force a win
			System.out.println(name + " is thinking...");
			String response = querySWI("canForceWin("+gameState.toPrologList()+", C).");
			
			if(response == null || response.contains("true") || response.contains("yes")){
				throw new RuntimeException("Something went wrong. No or invalid output from swipl.");
			}else{				
				//check if swi found that there is no way to win
				if(response.contains("false") || response.contains("no")) {
					System.out.println("\"It seems seems that the only winning move is to not play.\"");
					System.out.println("You must either be a living god or a Ðïr†¥ Hå¢kêr (or our prolog script is broken :'( ).");
					return -1;
				}else{
					//try to parse swi's column output
					try{
						int column = Integer.parseInt(response.substring(response.indexOf('=')+2, response.length()-1).trim());
						System.out.println(name+" picks column #"+column+". Your move ;-)");
						return column;
					}catch(NumberFormatException e){
						throw new RuntimeException("SWI output "+response+" when a number was expected.", e);
					}
				}		
			}
			
			
		}
		
		/**
		 * Runs a Prolog query and waits for the output
		 * 
		 * @param query
		 * @return the first result of the query
		 */
		private String querySWI(String query){
			//query swi
			thinking = true;
			System.out.println(query);
			issueSWICommand(query);

			//wait for the result
			if(thinking) synchronized(LOCK){
				while(true){
					try{
						LOCK.wait();
						break;						
					}catch(InterruptedException e){
						e.printStackTrace();
					}
				}				
			}
			
			//once the player has been notified of an output from swi, wait until swiOutput isn't being used, and get the result.
			synchronized(swiOutput){
				//check if for some reason the output stack is empty
				if(swiOutput.empty()){
					return null;
				}else{
					//get the result and discard anything else that may 
					String result = swiOutput.pop();

					if(result.trim().length()==0) return null;
					
					swiOutput.clear();
					
					return result;
				}
			}
		}

		/**
		 * Runs a Prolog query in this object's SWI process
		 * 
		 * @param query
		 */
		private void issueSWICommand(String query){
			try{
				swiInput.write(query+"\n");
				swiInput.newLine();
				swiInput.flush();
			}catch(IOException e){
				//something done gone borked! Better let everyone know about it
				throw new RuntimeException("Error issuing query \""+query+"\" to swipl, did something kill the process?", e);
			}
		}
		
		@Override
		protected void finalize(){
			running  = false;
			//try killing normally
			issueSWICommand("halt.");
			
			try{
				swiInput.close();
			}catch(IOException e){
				e.printStackTrace();
			}
			swi.destroyForcibly();
		}

		@Override
		public void destroy(){
			finalize();
		}
		
	}
	
	
}
