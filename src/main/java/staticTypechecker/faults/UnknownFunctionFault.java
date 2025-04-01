package staticTypechecker.faults;

import jolie.lang.parse.context.ParsingContext;

/**
 * Fault thrown when trying to use an operation which has not been seen before.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class UnknownFunctionFault implements Fault {
	private String message;

	public UnknownFunctionFault(String message, ParsingContext ctx){
		this.message = FaultHandler.getFaultContextMessage(ctx) + message;
	}
	
	public String getMessage(){
		return this.message;
	}

	public int hashCode(){
		return this.message.hashCode();
	}

	public boolean equals(Object other){
		if(this == other){
			return true;
		}

		if(!this.getClass().equals(other.getClass())){
			return false;
		}

		Fault otherFault = (Fault)other;
		return this.getMessage().equals(otherFault.getMessage());
	}
}
