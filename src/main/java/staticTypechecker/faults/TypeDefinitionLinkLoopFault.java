package staticTypechecker.faults;

import jolie.lang.parse.context.ParsingContext;

/**
 * Fault thrown when a type contains itself in the top layer of the definition. 
 * Examples:
 * 	type A: A
 * 
 * 	type B: int | string | B
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class TypeDefinitionLinkLoopFault implements Fault {
	private String typeName;
	private ParsingContext ctx;

	public TypeDefinitionLinkLoopFault(String typeName, ParsingContext ctx){
		this.typeName = typeName;
		this.ctx = ctx;
	}

	public String getMessage(){
		return FaultHandler.getFaultContextMessage(ctx) + "Type definition link loop detected: " + this.typeName + " (this might mean that the referred type has not been defined or could not be retrieved)";
	}

	public int hashCode(){
		return this.typeName.hashCode() + this.ctx.hashCode();
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
