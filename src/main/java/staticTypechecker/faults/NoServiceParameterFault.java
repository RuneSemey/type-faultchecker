package staticTypechecker.faults;

import jolie.lang.parse.context.ParsingContext;
import staticTypechecker.entities.Service;

/**
 * Fault thrown when a service is being embedded without a parameter.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class NoServiceParameterFault implements Fault {
	private Service service;
	private ParsingContext ctx;
	private String message;

	public NoServiceParameterFault(Service service, ParsingContext ctx){
		this.service = service;
		this.ctx = ctx;
		this.message = FaultHandler.getFaultContextMessage(this.ctx) + "no parameter passed to the service \"" + this.service.name() + "\". Expected type:\n" + this.service.parameter().prettyString();
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
