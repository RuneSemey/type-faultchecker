package staticTypechecker;

import java.util.List;

import staticTypechecker.utils.ModuleHandler;
import staticTypechecker.entities.Operation;
import staticTypechecker.entities.SymbolTable;
import staticTypechecker.entities.Type;
import staticTypechecker.entities.Operation.OperationType;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.Interface;
import staticTypechecker.entities.Module;

public class InterfaceProcessorTester {
	public static boolean run(){
		String moduleName = AppTest.BASE_PATH + "testFilesForInterfaceProcessor/main.ol";
		List<Module> modules = ModuleHandler.loadModule(moduleName);
		AppTest.runProcessors(modules, 2);

		SymbolTable result = ModuleHandler.get(moduleName).symbols();
		SymbolTable target = new SymbolTable();

		// manually create the interface objects

		// MyInterface
		Interface myInterface = new Interface("MyInterface");
		myInterface.addOperation("myReqRes", new Operation("myReqRes", Type.INT(), Type.INT().addChild("x", Type.INT()).addChild("y", Type.STRING()), OperationType.REQRES));
		myInterface.addOperation("myOneWay", new Operation("myOneWay", Type.STRING(), null, OperationType.ONEWAY));
		myInterface.addOperation("mySecondOneWay", new Operation("mySecondOneWay", Type.BOOL(), null, OperationType.ONEWAY));

		target.put(SymbolTable.newPair("MyInterface", SymbolType.INTERFACE), myInterface);
		
		// ImportedInterface
		Interface importedInterface = new Interface("ImportedInterface");
		InlineType B = Type.INT();
		B.addChildUnsafe("x", B);
		importedInterface.addOperation("importedReqRes", new Operation("importedReqRes", Type.STRING().addChild("x", Type.INT()), B, OperationType.REQRES));
		importedInterface.addOperation("importedOneWay", new Operation("importedOneWay", Type.INT(), null, OperationType.ONEWAY));

		target.put(SymbolTable.newPair("ImportedInterface", SymbolType.INTERFACE), importedInterface);

		return AppTest.testSymbolsForEquality(result, target);
	}
}
