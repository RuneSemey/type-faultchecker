package staticTypechecker.entities;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Map.Entry;

import jolie.lang.NativeType;
import jolie.lang.parse.ast.types.BasicTypeDefinition;
import jolie.lang.parse.context.ParsingContext;
import jolie.util.Range;

/**
 * Represents a type in Jolie.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public abstract class Type implements Symbol {
	// basic types
	private final static InlineType BOOL = new InlineType(BasicTypeDefinition.of(NativeType.BOOL), new Range(1, 1), null, false);
	private final static InlineType INT = new InlineType(BasicTypeDefinition.of(NativeType.INT), new Range(1, 1), null, false);
	private final static InlineType LONG = new InlineType(BasicTypeDefinition.of(NativeType.LONG), new Range(1, 1), null, false);
	private final static InlineType DOUBLE = new InlineType(BasicTypeDefinition.of(NativeType.DOUBLE), new Range(1, 1), null, false);
	private final static InlineType STRING = new InlineType(BasicTypeDefinition.of(NativeType.STRING), new Range(1, 1), null, false);
	private final static InlineType VOID = new InlineType(BasicTypeDefinition.of(NativeType.VOID), new Range(1, 1), null, false);
	private final static InlineType ANY = new InlineType(BasicTypeDefinition.of(NativeType.ANY), new Range(1, 1), null, false);
	private final static InlineType UNDEFINED = new InlineType(BasicTypeDefinition.of(NativeType.ANY), null, null, true);

	public static InlineType BOOL(){ return Type.BOOL.shallowCopy(); };
	public static InlineType INT(){ return Type.INT.shallowCopy(); };
	public static InlineType LONG(){ return Type.LONG.shallowCopy(); };
	public static InlineType DOUBLE(){ return Type.DOUBLE.shallowCopy(); };
	public static InlineType STRING(){ return Type.STRING.shallowCopy(); };
	public static InlineType VOID(){ return Type.VOID.shallowCopy(); };
	public static InlineType ANY(){ return Type.ANY.shallowCopy(); };
	public static InlineType UNDEFINED(){ return Type.UNDEFINED.shallowCopy(); };

	public abstract boolean isSubtypeOf(Type other);
	public abstract boolean equals(Object other);

	public abstract ParsingContext context();

	public abstract Type shallowCopy();
	public abstract Type shallowCopyExcept(Path p);
	public abstract Type copy();
	public abstract Type copy(IdentityHashMap<Type, Type> rec);

	public abstract String prettyString();
	public abstract String prettyString(int level);
	public abstract String prettyString(int level, IdentityHashMap<Type, Void> recursive);

	/**
	 * Finds the type(s) of the given node.
	 * @param node the node to find the type(s) of.
	 * @return an ArrayList of BasicTypeDefinitions corresponding to the type(s) of the specified node.
	 */
	public static ArrayList<BasicTypeDefinition> getBasicTypesOfNode(Type node){
		ArrayList<BasicTypeDefinition> types = new ArrayList<>();
		
		if(node instanceof InlineType){
			InlineType parsedTree = (InlineType)node;
			types.add(parsedTree.basicType());
		}
		else{
			ChoiceType parsedTree = (ChoiceType)node;
			parsedTree.choices().forEach(c -> {
				types.addAll( Type.getBasicTypesOfNode(c) );
			});
		}

		return types;
	}

	/**
	 * Returns the deep copied version of t2 onto t1, that is t1 << t2.
	 * @param t1 the Type on the left side of the deep copy.
	 * @param t2 the Type on the right side of the deep copy.
	 * @return the result of t1 << t2.
	 */
	public static Type deepCopy(Type t1, Type t2){
		if(t1 == null){
			return t2.shallowCopy();
		}
		if(t2 == null){
			return t1.shallowCopy();
		}

		return Type.deepCopyRec(t1, t2.copy());
	}

	public static Type deepCopyRec(Type t1, Type t2){
		if(t1 instanceof InlineType && t2 instanceof InlineType){ // both are inline types, take t2 and add the children of t1 if they are not already in t2
			InlineType p1 = (InlineType)t1;
			InlineType p2 = (InlineType)t2;
			InlineType result = p2;

			for(Entry<String, Type> ent : p1.children().entrySet()){
				result.addChildIfAbsentUnsafe(ent.getKey(), ent.getValue());
			}

			return result;
		}
		else{ // both are choice types
			ChoiceType result = new ChoiceType();
			result.addChoiceUnsafe(t1);
			result.addChoiceUnsafe(t2);
			return result;
		}
	}
}
