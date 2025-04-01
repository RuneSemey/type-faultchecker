package staticTypechecker.entities;

import java.util.HashSet;

/**
 * Represents an output port in Jolie.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class OutputPort implements Symbol {
	private String name; 					// the name of the port
	private String location; 				// the location of the port
	private String protocol; 				// the protocol of the port
	private HashSet<Interface> interfaces; 	// a list of the interfaces this output port uses

	public OutputPort(String name, String location, String protocol, HashSet<Interface> interfaces){
		this.name = name;
		this.location = location;
		this.protocol = protocol;
		this.interfaces = interfaces;
	}

	/**
	 * @return the name of this OutputPort.
	 */
	public String name(){
		return this.name;
	}

	/**
	 * @return the location of this OutputPort.
	 */
	public String location(){
		return this.location;
	}

	/**
	 * @return the protocol of this OutputPort.
	 */
	public String protocol(){
		return this.protocol;
	}

	/**
	 * @return the interfaces of this OutputPort.
	 */
	public HashSet<Interface> interfaces(){
		return this.interfaces;
	}

	/**
	 * Set the name of this OutputPort.
	 * @param name the new name.
	 */
	public void setName(String name){
		this.name = name;
	}

	/**
	 * Set the location of this OutputPort.
	 * @param location the new location.
	 */
	public void setLocation(String location){
		this.location = location;
	}

	/**
	 * Set the protocol of this OutputPort.
	 * @param protocol the new protocol.
	 */
	public void setProtocol(String protocol){
		this.protocol = protocol;
	}

	/**
	 * Set the interfaces of this OutputPort.
	 * @param interfaces the new interfaces.
	 */
	public void setInterfaces(HashSet<Interface> interfaces){
		this.interfaces = interfaces;
	}

	/**
	 * Checks if this OutputPort contains an operation with the given name.
	 * @param name the name to look for.
	 * @return true if any of the interfaces in this OutputPort contains an operation with the given name, false otherwise.
	 */
	public boolean containsOperation(String name){
		for(Interface i : this.interfaces){
			if(i.containsOperation(name)){
				return true;
			}
		}
		return false;
	}

	/**
	 * @param name the operation name to look for.
	 * @return the operation of this OutputPort with the given name, if it is present, null otherwise.
	 */
	public Operation getOperation(String name){
		for(Interface i : this.interfaces){
			if(i.containsOperation(name)){
				return i.getOperation(name);
			}
		}
		return null;
	}

	public int hashCode(){
		int hashcode = this.name.hashCode();

		if(this.location != null){
			hashcode += 31 * this.location.hashCode();
		}
		if(this.protocol != null){
			hashcode += Math.pow(31, 2) * this.protocol.hashCode();
		}
		if(this.interfaces != null){
			hashcode += Math.pow(31, 3) * this.interfaces.hashCode();
		}
		return hashcode;
	}

	public boolean equals(Object other){
		if(this == other){
			return true;
		}

		if(!(other instanceof OutputPort)){
			return false;
		}

		OutputPort parsedOther = (OutputPort)other;

		if(!(this.name.equals(parsedOther.name) && this.location.equals(parsedOther.location) && this.protocol.equals(parsedOther.protocol))){
			return false;
		}

		if(this.interfaces.size() != parsedOther.interfaces.size()){
			return false;
		}

		for(Interface i : this.interfaces){
			if(!parsedOther.interfaces.contains(i)){
				return false;
			}
		}

		return true;
	}
	
	/** 
	* @return a nice textual representation of this OutputPort.
	*/
	public String prettyString(){
		return this.prettyString(0);
	}

	public String prettyString(int level){
		String res = "\t".repeat(level);
		
		if(this.name == null){
			return res + "null";
		}
		
		res += this.name + " at " + this.location + " via " + this.protocol + " exposing [";

		if(this.interfaces.size() == 0){
			res += "]";
		}
		else{
			for(Interface i : this.interfaces){
				res += "\n" + i.prettyString(level+1);
			}

			res += "\n" + "\t".repeat(level) + "]";
		}

		return res;
	}
}
