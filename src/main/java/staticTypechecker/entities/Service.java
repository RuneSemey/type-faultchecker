package staticTypechecker.entities;

import java.util.Map.Entry;

import java.util.Collection;
import java.util.HashMap;

/**
 * Represents a service in Jolie.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class Service implements Symbol {
	private String name; 								// the name of the service
	private HashMap<String, InputPort> inputPorts; 		// map names to input ports
	private HashMap<String, OutputPort> outputPorts; 	// map names to output ports
	private Type parameter; 							// service parameter

	public Service(){
		this.name = null;
		this.inputPorts = new HashMap<>();
		this.outputPorts = new HashMap<>();
		this.parameter = null;
	}

	/**
	 * @return the name of this Service.
	 */
	public String name(){
		return this.name;
	}

	/**
	 * Set the name of this Service.
	 * @param name the new name.
	 */
	public void setName(String name){
		this.name = name;
	}

	/**
	 * @param name the name of the InputPort to look for.
	 * @return the InputPort in this Service with the given name if it is present, false otherwise.
	 */
	public InputPort getInputPort(String name){
		return this.inputPorts.get(name);
	}

	/**
	 * @param name the name of the Operation to look for.
	 * @return the Operation with the given name in any InputPort of this Service.
	 */
	public Operation getOperation(String name){
		for(InputPort ip : this.inputPorts.values()){
			if(ip.containsOperation(name)){
				return ip.getOperation(name);
			}
		}
		return null;
	}

	/**
	 * @param name the name of the OutputPort to look for.
	 * @return the OutputPort of this Service with the given name if it is present, null otherwise.
	 */
	public OutputPort getOutputPort(String name){
		return this.outputPorts.get(name);
	}

	/**
	 * Adds the given InputPort with the given name.
	 * @param name the name of the InputPort.
	 * @param port the InputPort.
	 */
	public void addInputPort(String name, InputPort port){
		this.inputPorts.put(name, port);
	}

	/**
	 * Adds the given OutputPort with the given name.
	 * @param name the name of the OutputPort.
	 * @param port the OutputPort.
	 */
	public void addOutputPort(String name, OutputPort port){
		this.outputPorts.put(name, port);
	}

	/**
	 * @return the InputPorts of this Service.
	 */
	public Collection<Entry<String, InputPort>> inputPorts(){
		return this.inputPorts.entrySet();
	}

	/**
	 * @return the parameter of this Service.
	 */
	public Type parameter(){
		return this.parameter;
	}

	/**
	 * Set the parameter of this Service.
	 * @param parameter the new parameter.
	 */
	public void setParameter(Type parameter){
		this.parameter = parameter;
	}

	public String prettyString(){
		return this.prettyString(0);
	}

	public String prettyString(int level){
		String ret = this.name;

		if(this.parameter != null){
			String param = this.parameter.prettyString(level+1);
			if(param.contains("\n")){
				ret += "(\n" + "\t".repeat(level+1) + param + "\n" + "\t".repeat(level) + ")";
			}
			else{
				ret += "(" + param + ")";
			}
		}

		if(!this.inputPorts.isEmpty()){
			ret += "\n" + "\t".repeat(level+1) + "InputPorts:";
			for(Entry<String, InputPort> ent : this.inputPorts.entrySet()){
				ret += "\n" + ent.getValue().prettyString(level+2);
			}
		}

		if(!this.inputPorts.isEmpty()){
			ret += "\n" + "\t".repeat(level+1) + "OutputPorts:";
			for(Entry<String, OutputPort> ent : this.outputPorts.entrySet()){
				ret += "\n" + ent.getValue().prettyString(level+2);
			}
		}

		return ret;
	}
}
