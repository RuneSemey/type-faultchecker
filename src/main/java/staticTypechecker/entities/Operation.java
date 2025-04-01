package staticTypechecker.entities;

/**
 * Represents an operation in Jolie.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class Operation implements Symbol {
	public enum OperationType{
		ONEWAY, REQRES
	}

	private String name; 					// the name of the operation
	private Type requestType; 				// the name of the type of the input
	private Type responseType; 				// the name of the type of the output
	private OperationType operationType; 	// the type of operation (oneway or reqres)

	public Operation(String name, Type requestType, Type responseType, OperationType operationType){
		this.name = name;
		this.requestType = requestType == null ? Type.VOID() : requestType;
		this.responseType = responseType == null ? Type.VOID() : responseType;
		this.operationType = operationType;
	}

	/**
	 * @return the name of this Operation.
	 */
	public String name(){
		return this.name;
	}

	/**
	 * @return the request type of this Operation.
	 */
	public Type requestType(){
		return this.requestType;
	}

	/**
	 * @return the response type of this Operation.
	 */
	public Type responseType(){
		return this.responseType;
	}

	/**
	 * @return the type of this Operation.
	 */
	public OperationType type(){
		return this.operationType;
	}

	/**
	 * Set the name of this Operation.
	 * @param name the new name.
	 */
	public void setName(String name){
		this.name = name;
	}
	
	/**
	 * Set the request type of this Operation.
	 * @param requestType the new requestType.
	 */
	public void setRequestType(Type requestType){
		this.requestType = requestType;
	}

	/**
	 * Set the response type of this Operation.
	 * @param responseType the new responseType.
	 */
	public void setResponseType(Type responseType){
		this.responseType = responseType;
	}

	/**
	 * Set the operation type of this Operation.
	 * @param operationType the new operation type.
	 */
	public void setOperationType(OperationType operationType){
		this.operationType = operationType;
	}

	public int hashCode(){
		return (int)(this.name.hashCode() + 31 * this.requestType.hashCode() + Math.pow(31, 2) * (this.responseType == null ? 0 : this.responseType.hashCode()) + this.operationType.hashCode());
	}

	public boolean equals(Object other){
		if(this == other){
			return true;
		}
		
		if(!(other instanceof Operation)){
			return false;
		}
		
		Operation parsedOther = (Operation)other;

		boolean isEqual = 	this.name.equals(parsedOther.name) && 
							this.operationType.equals(parsedOther.operationType) && 
							this.requestType.equals(parsedOther.requestType);

		if(this.operationType == OperationType.REQRES){
			isEqual = isEqual && this.responseType.equals(parsedOther.responseType);
		}

		return isEqual;
	}

	/**
	 * Checks for compatibility between the two Operations. This means that they fulfill the following conditions:
	 * - same name
	 * - same type
	 * - request type of this Operation must be subtype of other's request type
	 * - response type of this Operation must be subtype of other's response type (in case of REQ-RES)
	 * @param other the Operation to check compatibility.
	 * @return true if this Operation is compatible with the given Operation, false otherwise.
	 */
	public boolean isCompatibleWith(Operation other){
		Operation parsedOther = (Operation)other;

		boolean isEqual = 	this.name.equals(parsedOther.name) && 
							this.operationType.equals(parsedOther.operationType) && 
							this.requestType.isSubtypeOf(parsedOther.requestType);

		if(this.operationType == OperationType.REQRES){
			isEqual = isEqual && this.responseType.isSubtypeOf(parsedOther.responseType);
		}

		return isEqual;
	}

	public String prettyString(){
		return prettyString(0);
	}

	public String prettyString(int level){
		String ret = "\t".repeat(level);

		if(this.name == null){
			return ret + "null";
		}

		ret += this.name;

		String request = this.requestType().prettyString(level+1);
		if(request.contains("\n")){
			ret += "\n" + "\t".repeat(level) + "(\n" + "\t".repeat(level+1) + request + "\n" + "\t".repeat(level) + ")";
		}
		else{
			ret += "(" + request + ")";
		}

		if(this.operationType == OperationType.REQRES){
			String response = this.requestType().prettyString(level+1);
			if(response.contains("\n")){
				ret += "\n" + "\t".repeat(level) + "(\n" + "\t".repeat(level+1) + response + "\n" + "\t".repeat(level) + ")";
			}
			else{
				ret += "(" + response + ")";
			}
		}

		return ret;
	}
}
