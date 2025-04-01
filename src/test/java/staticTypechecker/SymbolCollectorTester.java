package staticTypechecker;

import java.util.List;
import java.util.Map.Entry;

import staticTypechecker.entities.SymbolTable.Pair;
import staticTypechecker.entities.Symbol;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.entities.SymbolTable;
import staticTypechecker.entities.Module;
import staticTypechecker.utils.ModuleHandler;

public class SymbolCollectorTester {
	public static boolean run(){
		String moduleName = AppTest.BASE_PATH + "testFilesForSymbolChecker/main.ol";
		List<Module> modules = ModuleHandler.loadModule(moduleName);
		AppTest.runProcessors(modules, 0);

		SymbolTable result = modules.get(0).symbols();
		SymbolTable target = new SymbolTable();

		target.put(SymbolTable.newPair("penguin", SymbolType.TYPE), null);
		target.put(SymbolTable.newPair("EmbedMe", SymbolType.SERVICE), null);
		target.put(SymbolTable.newPair("MyInterface", SymbolType.INTERFACE), null);
		// target.put(SymbolTable.newPair("helloReqRes", SymbolType.OPERATION), null);
		// target.put(SymbolTable.newPair("helloOneway", SymbolType.OPERATION), null);
		target.put(SymbolTable.newPair("Y", SymbolType.TYPE), null);
		target.put(SymbolTable.newPair("X", SymbolType.TYPE), null);
		target.put(SymbolTable.newPair("paramType", SymbolType.TYPE), null);
		target.put(SymbolTable.newPair("MyService", SymbolType.SERVICE), null);
		target.put(SymbolTable.newPair("param", SymbolType.TYPE), null);
		target.put(SymbolTable.newPair("inputPort", SymbolType.INPUT_PORT), null);
		target.put(SymbolTable.newPair("outputPort", SymbolType.OUTPUT_PORT), null);
		target.put(SymbolTable.newPair("embedMe", SymbolType.OUTPUT_PORT), null);

		for(Entry<Pair<String, SymbolType>, Symbol> ent : target.entrySet()){
			Pair<String, SymbolType> key = ent.getKey();
			
			if(!result.containsKey(key)){
				System.out.println("symbol table does not include " + key);
				return false;
			}
		}

		return true;
	}
}
