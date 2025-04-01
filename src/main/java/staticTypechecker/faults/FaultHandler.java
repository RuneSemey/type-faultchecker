package staticTypechecker.faults;

import java.util.HashSet;

import jolie.lang.parse.context.ParsingContext;

/**
 * Handles all faults which may occur during the type checking.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class FaultHandler {
	private static HashSet<Fault> faults = new HashSet<>();

	/**
	 * Throw the given Fault.
	 * @param fault the Fault to throw.
	 * @param terminate indicates whether to terminate the execution of the type checker or not.
	 */
	public static void throwFault(Fault fault, boolean terminate){
		if(!FaultHandler.faults.contains(fault)){
			FaultHandler.faults.add(fault);
		}

		if(terminate){
			FaultHandler.printFaults();
			System.exit(0);
		}
	}

	/**
	 * @return true if there are no Faults present, false otherwise.
	 */
	public static boolean isEmpty(){
		return FaultHandler.faults.isEmpty();
	}

	/**
	 * @param fault the Fault to look for.
	 * @return true if the given Fault is present in the FaultHandler, false otherwise.
	 */
	public static boolean contains(Fault fault){
		for(Fault f : FaultHandler.faults){
			if(Fault.equals(f, fault)){
				return true;
			}
		}
		return false;
	}

	/**
	 * Prints all Faults to std:out.
	 */
	public static void printFaults(){
		for(Fault f : FaultHandler.faults){
			System.out.println(f.getMessage());
			System.out.println();
		}
	}

	/**
	 * @param ctx the ParsingContext to generate the fault message prefix.
	 * @return a fault message prefix specifying the file and line number.
	 */
	public static String getFaultContextMessage(ParsingContext ctx){
		if(ctx == null){
			return "";
		}
		
		return "Critical error in file '" + ctx.sourceName() + "' on line " + ctx.line() + ":\n";
	}
}
