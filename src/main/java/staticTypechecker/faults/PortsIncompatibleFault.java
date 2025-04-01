package staticTypechecker.faults;

import java.util.List;

import jolie.lang.parse.context.ParsingContext;
import staticTypechecker.entities.Operation;
import staticTypechecker.entities.OutputPort;
import staticTypechecker.entities.Service;

/**
 * Fault thrown when two ports are incompatible. In particular, when a service is being embedded in an existing outputPort and the inputPorts of the embedded service is not compatible with the outputPort.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class PortsIncompatibleFault implements Fault {
	private OutputPort host;
	private Service embeddedService;
	private List<Operation> unprovidedOperations;
	private ParsingContext ctx;
	private String message;

	public PortsIncompatibleFault(OutputPort host, Service embeddedService, List<Operation> unprovidedOperations, ParsingContext ctx){
		this.host = host;
		this.embeddedService = embeddedService;
		this.unprovidedOperations = unprovidedOperations;
		this.ctx = ctx;
		this.message = "";
	}

	public PortsIncompatibleFault(String message){
		this.message = message;
	}

	public String getMessage(){
		if(this.message.equals("")){
			String res = FaultHandler.getFaultContextMessage(this.ctx) + "service '" + this.embeddedService.name() + "' does not provide operations:\n";
			
			for(Operation op : this.unprovidedOperations){
				res += "\t" + op.prettyString() + "\n";
			}
			res +=  "required by port '" + this.host.name() + "'";
	
			return res;
		}

		return this.message;
	}

	public int hashCode(){
		return this.host.hashCode() + this.embeddedService.hashCode() + unprovidedOperations.hashCode() + ctx.hashCode();
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
