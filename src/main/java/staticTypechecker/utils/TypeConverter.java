package staticTypechecker.utils;

import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Map.Entry;

import jolie.lang.parse.ast.types.TypeChoiceDefinition;
import jolie.lang.parse.ast.types.TypeDefinition;
import jolie.lang.parse.ast.types.TypeDefinitionLink;
import jolie.lang.parse.ast.types.TypeDefinitionUndefined;
import jolie.lang.parse.ast.types.TypeInlineDefinition;
import staticTypechecker.entities.ChoiceType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.Symbol;
import staticTypechecker.entities.SymbolTable;
import staticTypechecker.entities.Type;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.faults.FaultHandler;
import staticTypechecker.faults.TypeDefinitionLinkLoopFault;

/**
 * A static converter for the existing Jolie types. Converts them to my custom types used in the static typechecking, namely InlineTypes and ChoiceTypes.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class TypeConverter {
	/**
	 * Creates a structure instance representing the structure of the given type.
	 * @param type the type to create the structure from.
	 * @return the structure instance representing the specified type.
	 */
	public static Type convert(TypeDefinition type, SymbolTable symbols){
		return convert(type, new IdentityHashMap<>(), symbols);
	}

	private static Type convert(TypeDefinition type, IdentityHashMap<TypeDefinition, Type> rec, SymbolTable symbols){
		if(type instanceof TypeInlineDefinition){
			return TypeConverter.convert((TypeInlineDefinition)type, rec, symbols);
		}

		if(type instanceof TypeChoiceDefinition){
			return TypeConverter.convert((TypeChoiceDefinition)type, rec, symbols);
		}

		if(type instanceof TypeDefinitionLink){
			return TypeConverter.convert((TypeDefinitionLink)type, rec, symbols);
		}

		if(type instanceof TypeDefinitionUndefined){
			return TypeConverter.convert((TypeDefinitionUndefined)type, rec, symbols);
		}

		return null;
	}

	private static InlineType convert(TypeInlineDefinition type, IdentityHashMap<TypeDefinition, Type> rec, SymbolTable symbols){
		if(rec.containsKey(type)){
			return (InlineType)rec.get(type);
		}

		InlineType result = new InlineType(type.basicType(), type.cardinality(), type.context(), type.untypedSubTypes());

		if(symbols.containsKey(type.name())){ // it is a known type
			Symbol symbol = symbols.get(type.name(), SymbolType.TYPE);

			if(symbol == null){ // type has not been in initialized, we init it here
				symbols.put(SymbolTable.newPair(type.name(), SymbolType.TYPE), result);
			}
			else if(symbol instanceof InlineType){ // check that the symbol is of the right type, it may be a choice type, if this node's parent is a choice, this node will have the same name as the parent, and thus the symbol table will hold the parent under this name
					
				return (InlineType)symbol;
			}
		}
		rec.put(type, result);

		if(type.subTypes() != null){ // type has children
			String UUID = "!" + System.identityHashCode(result); // unique name for this node
			result.addChildUnsafe(UUID, Type.VOID()); // this child is added to make the type unique if added to a chocie type (necessary in the case of: type A: int { x: A | int }). The A choice of x is added as recursive edge BEFORE the child x is added to the node, and thus it will be equivalent to just int, and the resulting choice will only have one choice, namely the int
			for(Entry<String, TypeDefinition> ent : type.subTypes()){
				String childName = ent.getKey();
				Type child = TypeConverter.convert(ent.getValue(), rec, symbols);

				result.addChildUnsafe(childName, child);
			}
			result.removeChildUnsafe(UUID);
		}

		return result;
	}

	private static ChoiceType convert(TypeChoiceDefinition type, IdentityHashMap<TypeDefinition, Type> rec, SymbolTable symbols){
		if(rec.containsKey(type)){
			return (ChoiceType)rec.get(type);
		}

		ChoiceType result = new ChoiceType();
		rec.put(type, result);

		if(symbols.containsKey(type.name())){ // it is a known type
			ChoiceType st = (ChoiceType)symbols.get(type.name(), SymbolType.TYPE);
			
			if(st == null){ // type has not been in initialized, we init it here
				symbols.put(SymbolTable.newPair(type.name(), SymbolType.TYPE), result);
			}
			else{ // otherwise we can use it
				return st;
			}
		}

		HashSet<TypeInlineDefinition> choices = TypeConverter.getChoices(type);
		for(TypeInlineDefinition def : choices){
			InlineType t = TypeConverter.convert(def, new IdentityHashMap<>(rec), symbols);
			result.addChoiceUnsafe(t);
		}

		return result;
	}

	private static Type convert(TypeDefinitionLink type, IdentityHashMap<TypeDefinition, Type> rec, SymbolTable symbols){
		return TypeConverter.convert(type.linkedType(), rec, symbols);
	}

	private static Type convert(TypeDefinitionUndefined type, IdentityHashMap<TypeDefinition, Type> rec, SymbolTable symbols){
		return null;
	}

	private static HashSet<TypeInlineDefinition> getChoices(TypeDefinition type){
		HashSet<TypeInlineDefinition> choices = new HashSet<>();
		TypeConverter.getChoicesRec(type, choices, type);
		return choices;
	}
	
	private static void getChoicesRec(TypeDefinition type, HashSet<TypeInlineDefinition> list, TypeDefinition root){
		if(type instanceof TypeChoiceDefinition){
			TypeConverter.getChoicesRec(((TypeChoiceDefinition)type).left(), list, root);
			TypeConverter.getChoicesRec(((TypeChoiceDefinition)type).right(), list, root);
		}
		else if(type instanceof TypeInlineDefinition){
			list.add((TypeInlineDefinition)type);
		}
		else if(type instanceof TypeDefinitionLink){
			TypeDefinition linkedType = ((TypeDefinitionLink)type).linkedType();

			if(linkedType == root){
				FaultHandler.throwFault(new TypeDefinitionLinkLoopFault(root.name(), root.context()), true);
			}

			TypeConverter.getChoicesRec(linkedType, list, root);
		}
		else{
			System.out.println("CONVERTION NOT SUPPORTED");
		}
	}
}
