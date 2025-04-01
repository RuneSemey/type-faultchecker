package staticTypechecker.utils;

import jolie.lang.Constants.OperandType;

import java.util.ArrayList;
import java.util.List;

import jolie.lang.NativeType;
import jolie.lang.parse.ast.types.BasicTypeDefinition;
import jolie.lang.parse.context.ParsingContext;
import staticTypechecker.faults.WarningHandler;
import staticTypechecker.entities.ChoiceType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.Type;

/**
 * Utily functions for working with basic types.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class BasicTypeUtils {
	/**
	 * @param operand the operand type
	 * @param t1 first input
	 * @param t2 second input
	 * @param ctx the parsing context of the operation, used for display of warnings
	 * @return the type of the result of the given operation on the given input types 
	 */
	public static List<BasicTypeDefinition> deriveTypeOfOperation(OperandType operand, Type t1, Type t2, ParsingContext ctx){
		if(t1 instanceof InlineType && t2 instanceof InlineType){
			return BasicTypeUtils.deriveTypeOfOperation(operand, (InlineType)t1, (InlineType)t2, ctx);
		}
		else if(t1 instanceof InlineType && t2 instanceof ChoiceType){
			return BasicTypeUtils.deriveTypeOfOperation(operand, (InlineType)t1, (ChoiceType)t2, ctx);
		}
		else if(t1 instanceof ChoiceType && t2 instanceof InlineType){
			return BasicTypeUtils.deriveTypeOfOperation(operand, (ChoiceType)t1, (InlineType)t2, ctx);
		}
		else{ // both are choice types
			return BasicTypeUtils.deriveTypeOfOperation(operand, (ChoiceType)t1, (ChoiceType)t2, ctx);
		}
	}

	/**
	 * Derive the type when both inputs are InlineTypes
	 */
	private static List<BasicTypeDefinition> deriveTypeOfOperation(OperandType operand, InlineType t1, InlineType t2, ParsingContext ctx){
		BasicTypeDefinition basicType = BasicTypeUtils.deriveTypeOfOperationOnBasicTypes(operand, t1.basicType(), t2.basicType(), ctx);
		// return new InlineType(basicType, null, null, false);
		ArrayList<BasicTypeDefinition> ret = new ArrayList<>();
		ret.add(basicType);
		return ret;
	}

	/**
	 * Derive the type of one InlineType and one ChoiceType as input
	 */
	private static List<BasicTypeDefinition> deriveTypeOfOperation(OperandType operand, InlineType t1, ChoiceType t2, ParsingContext ctx){
		ArrayList<BasicTypeDefinition> basicTypes = new ArrayList<>();

		for(InlineType choice : t2.choices()){
			basicTypes.add(BasicTypeUtils.deriveTypeOfOperationOnBasicTypes(operand, t1.basicType(), choice.basicType(), ctx));
		}

		// return ChoiceType.fromBasicTypes(basicTypes);
		return basicTypes;
	}

	/**
	 * Derive the type of one InlineType and one ChoiceType as input
	 */
	private static List<BasicTypeDefinition> deriveTypeOfOperation(OperandType operand, ChoiceType t1, InlineType t2, ParsingContext ctx){
		return BasicTypeUtils.deriveTypeOfOperation(operand, t2, t1, ctx);
	}

	/**
	 * Derive the type when both inputs are ChoiceTypes
	 */
	private static List<BasicTypeDefinition> deriveTypeOfOperation(OperandType operand, ChoiceType t1, ChoiceType t2, ParsingContext ctx){
		ArrayList<BasicTypeDefinition> basicTypes = new ArrayList<>();

		for(InlineType choice1 : t1.choices()){
			for(InlineType choice2 : t2.choices()){
				basicTypes.add(BasicTypeUtils.deriveTypeOfOperationOnBasicTypes(operand, choice1.basicType(), choice2.basicType(), ctx));
			}
		}

		// return ChoiceType.fromBasicTypes(basicTypes);
		return basicTypes;
	}

	/**
	 * Derives the type of the operation given two basic types
	 */
	public static BasicTypeDefinition deriveTypeOfOperationOnBasicTypes(OperandType operand, BasicTypeDefinition t1, BasicTypeDefinition t2, ParsingContext ctx){
		NativeType type1 = t1.nativeType();
		NativeType type2 = t2.nativeType();

		// if one of the types is void, return the other
		if(type1 == NativeType.VOID){
			return BasicTypeDefinition.of(type2);
		}
		else if(type2 == NativeType.VOID){
			return BasicTypeDefinition.of(type1);
		}

		// check all combinations and return the appropriate type (NOTE the appropriate types have been deducted by doing all these sums and seeing what the runtime interpreter derives them to)
		if(type1 == NativeType.BOOL){
			if(type2 == NativeType.STRING){
				if(operand == OperandType.ADD){ // adding a bool with a string results in string
					return BasicTypeDefinition.of(NativeType.STRING);
				}
				else{ // all other cases are bools
					return BasicTypeDefinition.of(NativeType.BOOL);
				}
			}
			else{
				return BasicTypeDefinition.of(type2);
			}
		}
		else if(type1 == NativeType.INT){
			if(type2 == NativeType.BOOL || type2 == NativeType.INT){
				return BasicTypeDefinition.of(NativeType.INT);
			}
			else if(type2 == NativeType.LONG){
				return BasicTypeDefinition.of(NativeType.LONG);
			}
			else if(type2 == NativeType.DOUBLE){
				return BasicTypeDefinition.of(NativeType.DOUBLE);
			}
			else if(type2 == NativeType.STRING){
				if(operand == OperandType.ADD){
					return BasicTypeDefinition.of(NativeType.STRING);
				}
				if(operand == OperandType.SUBTRACT || operand == OperandType.MULTIPLY){
					return BasicTypeDefinition.of(NativeType.INT);
				}

				// throw warning, division and modulo with a string only allowed if string can be parsed to int
				WarningHandler.throwWarning("the operations 'int / string' and 'int % string' are only allowed if the string can be parsed to a number", ctx);
				return BasicTypeDefinition.of(NativeType.INT);
			}
		}
		else if(type1 == NativeType.LONG){
			if(type2 == NativeType.BOOL || type2 == NativeType.INT || type2 == NativeType.LONG){
				return BasicTypeDefinition.of(NativeType.LONG);
			}
			if(type2 == NativeType.DOUBLE){
				return BasicTypeDefinition.of(NativeType.DOUBLE);
			}
			if(type2 == NativeType.STRING){
				if(operand == OperandType.ADD){
					return BasicTypeDefinition.of(NativeType.STRING);
				}
				else if(operand == OperandType.SUBTRACT || operand == OperandType.MULTIPLY){
					return BasicTypeDefinition.of(NativeType.LONG);
				}

				// throw warning, division and modulo with a string only allowed if string can be parsed to int
				WarningHandler.throwWarning("the operations 'long / string' and 'long % string' are only allowed if the string can be parsed to a number", ctx);
				return BasicTypeDefinition.of(NativeType.LONG);
			}

		}
		else if(type1 == NativeType.DOUBLE){
			if(type2 == NativeType.BOOL || type2 == NativeType.INT || type2 == NativeType.LONG || type2 == NativeType.DOUBLE){
				return BasicTypeDefinition.of(NativeType.DOUBLE);
			}
			if(type2 == NativeType.STRING){
				if(operand == OperandType.ADD){
					return BasicTypeDefinition.of(NativeType.STRING);
				}
				
				return BasicTypeDefinition.of(NativeType.DOUBLE);
			}
		}
		else if(type1 == NativeType.STRING){
			if(type2 == NativeType.BOOL){
				if(operand == OperandType.MULTIPLY){
					return BasicTypeDefinition.of(NativeType.BOOL);
				}

				return BasicTypeDefinition.of(NativeType.STRING);
			}

			if(operand == OperandType.ADD){
				return BasicTypeDefinition.of(NativeType.STRING);
			}

			return BasicTypeDefinition.of(type2);
		}

		return BasicTypeDefinition.of(NativeType.VOID);
	}
}
