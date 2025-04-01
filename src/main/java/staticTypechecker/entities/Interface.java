package staticTypechecker.entities;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map.Entry;

import staticTypechecker.entities.Operation.OperationType;

/**
 * Represents an interface in Jolie.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class Interface implements Symbol {
	private String name; // the name of the interface
	private HashMap<String, Operation> operations; // maps operation names to their object

	public Interface(String name){
		this.name = name;
		this.operations = new HashMap<>();
	}

	/**
	 * Add an Operation to this interface with the given name.
	 * @param opName the name of the Operation.
	 * @param op the Operation.
	 */
	public void addOperation(String opName, Operation op){
		this.operations.put(opName, op);
	}

	/**
	 * Add an Operation to this interface with the name stored in the object.
	 * @param op the Operation.
	 */
	public void addOperation(Operation op){
		this.operations.put(op.name(), op);
	}

	/**
	 * @param name the operation name to look for.
	 * @return the Operation with the given name, if it is present in this Interface, null otherwise.
	 */
	public Operation getOperation(String name){
		return this.operations.get(name);
	}

	/**
	 * @param name the operation name to look for.
	 * @return true if this Interface contains an Operation with the given name, false otherwise.
	 */
	public boolean containsOperation(String name){
		return this.operations.containsKey(name);
	}

	/**
	 * @return the Operations of this Interface.
	 */
	public Collection<Entry<String, Operation>> operations(){
		return this.operations.entrySet();
	}

	/**
	 * @return the name of this Interface.
	 */
	public String name(){
		return this.name;
	}

	/**
	 * Set the name of this Interface.
	 * @param name the new name.
	 */
	public void setName(String name){
		this.name = name;
	}

	public int hashCode(){
		return this.name.hashCode() + 31 * this.operations.hashCode();
	}

	public boolean equals(Object other){
		if(this == other){
			return true;
		}
		
		if(!(other instanceof Interface)){
			return false;
		}

		Interface parsedOther = (Interface)other;
		for(Entry<String, Operation> ent : this.operations.entrySet()){
			String opName = ent.getKey();
			Operation op = ent.getValue();

			if(!parsedOther.operations.containsKey(opName)){
				return false;
			}
			if(!op.equals(parsedOther.operations.get(opName))){
				return false;
			}
		}

		return true;
	}

	public String prettyString(){
		return this.prettyString(0);
	}

	public String prettyString(int level){
		String res = "\t".repeat(level);

		if(this.name == null){
			return res + "null";
		}

		res += this.name;

		ArrayList<Operation> oneWay = new ArrayList<>();
		ArrayList<Operation> reqRes = new ArrayList<>();

		// split the operations based on type
		for(Entry<String, Operation> ent : this.operations.entrySet()){
			Operation op = ent.getValue();
			
			if(op.type() == OperationType.ONEWAY){ // of type one way
				oneWay.add(op);
			}
			else{ // of type req res
				reqRes.add(op);
			}
		}
		
		// add oneway operations if any
		res += "\n" + "\t".repeat(level+1) + "OneWay:";
		if(!oneWay.isEmpty()){
			for(Operation op : oneWay){
				res += "\n" + op.prettyString(level+2);
			}
		}

		// add req res operations if any
		res += "\n" + "\t".repeat(level+1) + "RequestResponse:";
		if(!reqRes.isEmpty()){
			for(Operation op : reqRes){
				res += "\n" + op.prettyString(level+2);
			}
		}

		return res;
	}
}