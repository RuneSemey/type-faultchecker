package staticTypechecker.faults;

/**
 * Simple interface for a fault used in the type checker.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public interface Fault {
	public String getMessage();
	public int hashCode();
	public boolean equals(Object other);

	public static boolean equals(Fault f1, Fault f2){
		if(f1 == f2){
			return true;
		}

		if(f1 == null || f2 == null){ // they cant both be null, since we already checked if their pointers are the same
			return false;
		}

		if(!f1.getClass().equals(f2.getClass())){
			return false;
		}
		
		return f1.getMessage().equals(f2.getMessage());
	}
}
