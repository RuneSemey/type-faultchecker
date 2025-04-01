package staticTypechecker.faults;

import jolie.lang.parse.context.ParsingContext;

/**
 * A type fault of some sort. Specified by the message provided.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class TypeFault implements Fault {
	private String message;

	public TypeFault(String message, ParsingContext ctx){
		this.message = FaultHandler.getFaultContextMessage(ctx) + message;
	}
	
	public String getMessage(){
		return message;
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
