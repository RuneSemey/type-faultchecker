package staticTypechecker.entities;

/**
 * Represents any entity in Jolie, such as a type, service, port etc.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public interface Symbol {
	/**
	 * @return a nice textual representation of this Symbol.
	 */
	public String prettyString();
	public String prettyString(int level);

	public static boolean equals(Symbol A, Symbol B){
		if(A == B){
			return true;
		}
		
		if(A instanceof InputPort){
			return ((InputPort)A).equals(B);
		}
		if(A instanceof Interface){
			return ((Interface)A).equals(B);
		}
		if(A instanceof Operation){
			return ((Operation)A).equals(B);
		}
		if(A instanceof OutputPort){
			return ((OutputPort)A).equals(B);
		}
		if(A instanceof Service){
			return ((Service)A).equals(B);
		}
		if(A instanceof Type){
			return ((Type)A).equals(B);
		}

		return false;
	}

	public enum SymbolType{
		TYPE, INTERFACE, OPERATION, SERVICE, INPUT_PORT, OUTPUT_PORT, ANY
	}
}
