package staticTypechecker.entities;

import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map.Entry;

import jolie.lang.parse.ast.types.BasicTypeDefinition;
import jolie.lang.parse.context.ParsingContext;
import jolie.util.Range;
import staticTypechecker.utils.Simulator;

/**
 * Represents an inline type in Jolie such as "type a: int { x: string }".
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class InlineType extends Type {
	private BasicTypeDefinition basicType; 	// the type of the root node
	private Range cardinality; 				// the cardinality of the root node
	private HashMap<String, Type> children; // the children of the root node, maps child names to the type
	private boolean openRecord; 			// indicates whether this type is an open or closed record 
	private ParsingContext context;			// the parsing context of this type

	public InlineType(BasicTypeDefinition basicType, Range cardinality, ParsingContext context, boolean openRecord){
		this.basicType = basicType;
		this.children = new HashMap<>();
		this.cardinality = cardinality;
		this.context = context;
		this.openRecord = openRecord;
	}

	/**
	 * @return the basic type of this InlineType.
	 */
	public BasicTypeDefinition basicType(){
		return this.basicType;
	}

	/**
	 * @return the cardinality of this InlineType.
	 */
	public Range cardinality(){
		return this.cardinality;
	}

	/**
	 * @return the parsing context of this InlineType.
	 */
	public ParsingContext context(){
		return this.context;
	}

	/**
	 * @return true if this InlineType is an open record, false otherwise.
	 */
	public boolean isOpen(){
		return this.openRecord;
	}

	/**
	 * @return true if this InlineType is NOT an open record, false otherwise. Equivalent to !isOpen().
	 */
	public boolean isClosed(){
		return !this.openRecord;
	}

	/**
	 * Overwrites the basic type of this InlineType. WARNING: alters this object.
	 * @param basicType the new basic type .
	 */
	public void setBasicTypeUnsafe(BasicTypeDefinition basicType){
		this.basicType = basicType;
	}

	/**
	 * Overwrites the children of this InlineType. WARNING: alters this object.
	 * @param children the new children.
	 */
	public void setChildrenUnsafe(HashMap<String, Type> children){
		this.children = new HashMap<>(children);
	}

	/**
	 * Adds the given children to this InlineType. WARNING: alters this object.
	 * @param children the children to add.
	 */
	public void addChildrenUnsafe(HashMap<String, Type> children){
		this.children.putAll(children);
	}

	/**
	 * Overwrites the cardinality of this InlineType. WARNING: alters this object.
	 * @param cardinality the new cardinality.
	 */
	public void setCardinalityUnsafe(Range cardinality){
		this.cardinality = cardinality;
	}

	/**
	 * Overwrites the context of this InlineType. WARNING: alters this object.
	 * @param context the new context.
	 */
	public void setContextUnsafe(ParsingContext context){
		this.context = context;
	}

	/**
	 * Overwrites the open status of this InlineType. WARNING: alters this object.
	 * @param openRecord the new open status.
	 */
	public void setOpenStatusUnsafe(boolean openStatus){
		this.openRecord = openStatus;
	}

	/**
	 * Sets the open status of this InlineType. NOTE: does not alter this object.
	 * @param openStatus the new open status.
	 * @return a shallow copy of this object with the open status changed to the given boolean.
	 */
	public InlineType setOpenStatus(boolean openStatus){
		InlineType copy = this.shallowCopy();
		copy.setOpenStatusUnsafe(openStatus);
		return copy;
	}

	/**
	 * Adds the entry {name, child} to this node. Owerwrites existing entries. WARNING: alters this object.
	 * @param name the name of the child (what key to associate it to).
	 * @param child the structure of the child.
	 */
	public void addChildUnsafe(String name, Type child){
		this.children.put(name, child);
	}

	/**
	 * Adds the entry {name, child} to this node if they key does not already exist. WARNING: may alter this object.
	 * @param name the name of the child (what key to associate it to).
	 * @param child the structure of the child.
	 */
	public void addChildIfAbsentUnsafe(String name, Type child){
		this.children.putIfAbsent(name, child);
	}

	/**
	 * Removes the child with the given name from this InlineType. WARNING: alters this object.
	 * @param name the name of the child to remove.
	 */
	public void removeChildUnsafe(String name){
		this.children.remove(name);
	}

	/**
	 * Set the basic type of this InlineType. NOTE: does not alter this object.
	 * @param basicType the new basic type.
	 * @return a shallow copy of this InlineType with the given basicType.
	 */
	public InlineType setBasicType(BasicTypeDefinition basicType){
		InlineType copy = this.shallowCopy();
		copy.setBasicTypeUnsafe(basicType);
		return copy;
	}

	/**
	 * Set the children of this InlineType, completely overwriting them. NOTE: does not alter this object.
	 * @param children the children.
	 * @return a shallow copy of this InlineType with the children overwritten by the given children.
	 */
	public InlineType setChildren(HashMap<String, Type> children){
		InlineType copy = this.shallowCopy();
		copy.setChildrenUnsafe(children);
		return copy;
	}

	/**
	 * Adds a child to this InlineType. NOTE: does not alter this object.
	 * @param child the child to add.
	 * @return a shallow copy of this InlineType with the given children added.
	 */
	public InlineType addChild(String name, Type child){
		InlineType copy = this.shallowCopy();
		copy.addChildUnsafe(name, child);
		return copy;
	}

	/**
	 * Adds multiple children to this InlineType. NOTE: does not alter this object.
	 * @param children the children to add.
	 * @return a shallow copy of this InlineType with the given children added.
	 */
	public InlineType addChildren(HashMap<String, Type> children){
		InlineType copy = this.shallowCopy();
		copy.addChildrenUnsafe(children);
		return copy;
	}

	/**
	 * @param name the name to look for.
	 * @return true if this InlineType has a child with the given name, false otherwise.
	 */
	public boolean contains(String name){
		return this.children.containsKey(name);
	}

	/**
	 * @param name the child name to look for.
	 * @return the child node, if it is present, null otherwise.
	 */
	public Type getChild(String name){
		if(name == "?" && this.isOpen()){
			return Type.UNDEFINED();
		}

		return this.children.get(name);
	}

	/**
	 * @return the children of this InlineType.
	 */
	public HashMap<String, Type> children(){
		return this.children;
	}

	/**
	 * Checks for equality.
	 * @param other the other object.
	 * @return true if the objects are structural equivalent and false otherwise.
	 */
	public boolean equals(Object other){
		if(this == other){
			return true;
		}
		
		if(!other.getClass().equals(this.getClass())){ // of different classes, they cannot be equivalent
			return false;
		}

		return Simulator.equivalent(this, (InlineType)other);
	}

	public boolean isSubtypeOf(Type other){
		if(this == other){
			return true;
		}

		return Simulator.isSubtypeOf(this, other);
	}

	/**
	 * @return a shallow copy of this InlineType (different root node same children).
	 */
	public InlineType shallowCopy(){
		InlineType copy = new InlineType(this.basicType, this.cardinality, this.context, this.openRecord);
		copy.setChildrenUnsafe(this.children);
		return copy;
	}

	/**
	 * @return a shallow copy of this InlineType. All children are the same as this InlineType, EXCEPT for the nodes in the given Path - those are also shallow copied.
	 */
	public Type shallowCopyExcept(Path p){
		if(p.isEmpty()){
			this.shallowCopy();
		}

		InlineType result = this.shallowCopy();
		String childName = p.get(0);
		
		if(this.contains(childName)){
			result.addChildUnsafe(childName, this.getChild(childName).shallowCopyExcept(p.remainder()));
		}
		
		return result;
	}

	/**
	 * Creates a deep copy of this structure.
	 * @return the deep copy.
	 */
	public InlineType copy(){
		return this.copy(new IdentityHashMap<>());
	}

	/**
	 * @param seenTypes is a HashMap which maps typestructures in the original type to their equivalent part in the copy. This is used when dealing with recursive types.
	 */
	public InlineType copy(IdentityHashMap<Type, Type> seenTypes){
		InlineType struct = new InlineType(this.basicType, this.cardinality, this.context, this.openRecord);
		seenTypes.put(this, struct);

		this.children.entrySet().forEach(child -> {
			String childName = child.getKey();
			Type childStruct = child.getValue();

			// run through the already seen types and see if we already copied this object, if so just use this copy
			for(Entry<Type, Type> seenType : seenTypes.entrySet()){
				if(seenType.getKey() == childStruct){
					struct.addChildUnsafe(childName, seenType.getValue());
					return; // return acts as continue, since we are using a stream
				}
			}

			// otherwise we must make a new copy
			struct.addChildUnsafe(childName, childStruct.copy(seenTypes));
		});
		
		return struct;
	}

	public int hashCode(){
		int hashCode = 1;

		if(this.basicType != null){
			hashCode += this.basicType.hashCode();
		}

		if(this.children != null){
			hashCode += this.children.size() * 7823; // 7823 is just a prime number
		}

		hashCode += Math.pow(31, this.openRecord ? 1 : 0);
		
		return hashCode;
	}

	/**
	 * @return a nice textual representation of this InlineType.
	 */
	public String prettyString(){
		return this.prettyString(0, new IdentityHashMap<>());
	}

	public String prettyString(int level){
		return this.prettyString(level, new IdentityHashMap<>());
	}

	public String prettyString(int level, IdentityHashMap<Type, Void> recursive){
		recursive.put(this, null);
		String result = "";
		
		// print the basic type
		if(this.basicType != null){
			result += this.basicType.nativeType().id();
		}
		else{
			result += "no type";
		}

		// result += " (" + System.identityHashCode(this) + ")";

		// print the children if any
		if(this.children.size() != 0){
			result += " {"; // open a bracket

			if(this.openRecord){
				result += "\n" + "\t".repeat(level+1) + "?";
			}

			for(Entry<String, Type> ent : this.children.entrySet()){
				String childName = ent.getKey();
				Type child = ent.getValue();

				if(childName == "?"){
					continue;
				}

				if(recursive.containsKey(child)){ // child have been printed before, it is recursive
					// result += "\n" + "\t".repeat(level+1) + childName + " (recursive edge to " + System.identityHashCode(child) + ")";
					result += "\n" + "\t".repeat(level+1) + childName + " (recursive edge)";
					continue;
				}

				IdentityHashMap<Type, Void> rec = new IdentityHashMap<>(recursive); // shallow copy to not pass the same to each chilf
				result += "\n" + "\t".repeat(level+1) + childName + ": " + (child instanceof InlineType ? child.prettyString(level+1, rec) : child.prettyString(level+2, rec));
			}

			result += "\n" + "\t".repeat(level) + "}"; // close the bracket again
		}
		else if(this.openRecord){
			result += " { ? }";
		}
		
		return result;
	}
}
