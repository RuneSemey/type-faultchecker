package staticTypechecker;

import java.util.Map.Entry;
import java.util.HashMap;

import jolie.lang.NativeType;
import staticTypechecker.utils.ModuleHandler;
import staticTypechecker.entities.SymbolTable.Pair;
import staticTypechecker.entities.Module;
import staticTypechecker.entities.Symbol;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.faults.FaultHandler;
import staticTypechecker.faults.WarningHandler;
import staticTypechecker.entities.Type;
import staticTypechecker.visitors.InputPortProcessor;
import staticTypechecker.visitors.InterfaceProcessor;
import staticTypechecker.visitors.OutputPortProcessor;
import staticTypechecker.visitors.SymbolCollector;
import staticTypechecker.visitors.Synthesizer;
import staticTypechecker.visitors.TypeProcessor;

/**
 * The main flow of the type checker. Here we clearly see the stages of the pipeline.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class Main {

	public static void main( String[] args ) {
		// set the default value of the typehints to assertions
		if(System.getProperty("typehint") == null){
			System.setProperty("typehint", "assertions");
		}

		// stage 0: parse the modules
		String moduleName = args[0];
		ModuleHandler.loadModule(moduleName);

		// stage 1: discover symbols in all modules
		SymbolCollector sCollector = new SymbolCollector();
		ModuleHandler.runVisitor(sCollector);

		// stage 2: process type definitions in all modules
		TypeProcessor tProcessor = new TypeProcessor();
		ModuleHandler.runVisitor(tProcessor);

		// stage 3: process interfaces in all modules
		InterfaceProcessor iProcessor = new InterfaceProcessor();
		ModuleHandler.runVisitor(iProcessor);

		// stage 4: process service-parameters and input ports in all services
		InputPortProcessor ipProcessor = new InputPortProcessor();
		ModuleHandler.runVisitor(ipProcessor);

		// stage 5: process output ports and embeddings in all services
		OutputPortProcessor opProcessor = new OutputPortProcessor();
		ModuleHandler.runVisitor(opProcessor);

		// stage 6: process service behaviors
		HashMap<String, Type> trees = new HashMap<>();
		
		ModuleHandler.modules().values().forEach(m -> {
			Synthesizer synth = Synthesizer.get(m);
			trees.put(m.name(), synth.synthesize().getType());
		});

		WarningHandler.printWarnings();
		FaultHandler.printFaults();
	}

	/**
	 * Utility function, prints all symbols of all modules in a structured manner.
	 */
	private static void printAllSymbols(){
		System.out.println("-----------------------------------------");
		for(Module m : ModuleHandler.modules().values()){
			System.out.println("Module: " + m.name());

			for(Entry<Pair<String, SymbolType>, Symbol> symbol : m.symbols().entrySet()){
				if(!NativeType.isNativeTypeKeyword(symbol.getKey().key()) && !symbol.getKey().key().equals("undefined")){ // we dont want to print the base types
					if(symbol.getValue() != null && symbol.getValue() != null){ // the symbol object have been initialized and can thus be pretty printed
						System.out.println("\n" + symbol.getKey() + ":\n" + symbol.getValue().prettyString());
					}
					else{
						System.out.println("\n" + symbol.getKey() + ":\n" + null);
					}
				}
			}

			System.out.println("______________");
		}
		System.out.println("-----------------------------------------");
	}

	/**
	 * Utility function, prints all symbol trees of all modules in a structured manner.
	 */
	private static void printTrees(HashMap<String, Type> trees){
		System.out.println("-----------------------------------------");
		for(Entry<String, Type> ent : trees.entrySet()){
			String moduleName = ent.getKey();
			Type tree = ent.getValue();

			System.out.println("Final tree for '" + moduleName + "':");
			System.out.println(tree.prettyString());
		}
		System.out.println("-----------------------------------------");
	}
}
