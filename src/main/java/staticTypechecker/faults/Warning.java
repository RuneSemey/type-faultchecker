package staticTypechecker.faults;

/**
 * Simple warning. Note warnings are not critical errors, but are simply meant to tell the programmer of potential errors, which we cannot determine at compile time
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class Warning {
	private String message;

	public Warning(String message){
		this.message = message;
	}

	public String message(){
		return this.message;
	}

	public String toString(){
		return this.message;
	}
}
