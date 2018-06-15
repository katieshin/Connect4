package net.connect4;

public final class Solver {

	private final byte width;
	private final byte height;
	
	private final char p1;
	private final char p2;	

	public Solver(int width, int height, char p1, char p2){
		this.width = (byte) width;
		this.height = (byte) height;
		this.p1 = p1;
		this.p2 = p2;
		
		Bitboard b = new Bitboard();
		System.out.println(b);
		b = b.addPiece(3);
		System.out.println(b);
		b = b.addPiece(3);
		System.out.println(b);
		b = b.addPiece(2);
		System.out.println(b);
		b = b.addPiece(4);
		System.out.println(b);
		b = b.addPiece(3);
		System.out.println(b);
		
		Bitboard b2 = new Bitboard(b.tokens >> height, b.filledSpots >> height);
		System.out.println(b2);
		
		b.tokens = b.tokens & b2.tokens;
		b.filledSpots = b.filledSpots & b2.filledSpots;
		
		System.out.println(b);
		
	}



	private class Bitboard {

		private long tokens;
		private long filledSpots;
		
		public Bitboard(){
			this(0,0);
		}
		
		public Bitboard(long tokens, long filledSpots){
			if(height*(width+1)>=64) throw new IllegalArgumentException("Cannot store a "+width+"x"+height+" board in a 64bit-bitboard.");
			this.tokens = tokens;
			this.filledSpots = filledSpots;
		}

		public Bitboard addPiece(int column){
			return canPlay(column) ?
					new Bitboard(tokens^filledSpots, filledSpots | (filledSpots + posMask(0, column))) : 
					null;
		}
		
		public boolean canPlay(int column){
			return (filledSpots & posMask(height-1, column)) == 0;
		}
		
		public char getTokenAt(int r, int c){
			if((filledSpots & posMask(r, c)) == 0) return ' ';
			return (tokens & posMask(r, c)) == 0 ? p1 : p2;
		}
		
		public long getKey(){
			long bottomRow = 0;
			for(int j=0; j<width; bottomRow|=posMask(0, j));
			return tokens + filledSpots + bottomRow;
		}
		
		
		private long posMask(int r, int c){
			return 1L << r << c*height;
		}
		
		@Override
		public String toString(){
			String s = "";
			for(int j=height-1; j>=0; j--) {
				s+="|";
				for(int i=0; i<width; i++) {
					s+=getTokenAt(j, i)+"|";
				}
				s+="\n";
			}
			return s;
		}
		
	}
	
}
