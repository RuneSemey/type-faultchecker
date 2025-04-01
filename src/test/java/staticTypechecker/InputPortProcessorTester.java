package staticTypechecker;

import java.util.HashSet;
import java.util.List;

import staticTypechecker.entities.SymbolTable;
import staticTypechecker.entities.Type;
import staticTypechecker.entities.Operation.OperationType;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.entities.ChoiceType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.InputPort;
import staticTypechecker.entities.Interface;
import staticTypechecker.entities.Module;
import staticTypechecker.entities.Operation;
import staticTypechecker.utils.ModuleHandler;

public class InputPortProcessorTester {
	public static boolean run(){
		String moduleName = AppTest.BASE_PATH + "testFilesForInputPortProcessor/main.ol";
		List<Module> modules = ModuleHandler.loadModule(moduleName);
		AppTest.runProcessors(modules, 3);

		SymbolTable result = ModuleHandler.get(moduleName).symbols();
		SymbolTable target = new SymbolTable();

		HashSet<Interface> interfacesInputPort1 = new HashSet<>();
		Interface i1 = new Interface("MyInterface1");
		i1.addOperation(new Operation("MyReqRes", Type.INT(), Type.STRING(), OperationType.REQRES));
		i1.addOperation(new Operation("MyOneWay", Type.BOOL(), null, OperationType.ONEWAY));
		interfacesInputPort1.add(i1);
		InputPort InputPort1 = new InputPort("InputPort1", "socket://localhost:8080", "sodep", interfacesInputPort1);

		target.put(SymbolTable.newPair("InputPort1", SymbolType.INPUT_PORT), InputPort1);

		HashSet<Interface> interfacesInputPort2 = new HashSet<>();
		Interface i2 = new Interface("MyInterface2");
		InlineType A = Type.ANY().addChild("x", Type.STRING());
		A.addChildUnsafe("y", A);
		ChoiceType B = new ChoiceType().addChoice(Type.INT()).addChoice(Type.STRING()); 
		i2.addOperation(new Operation("MyReqRes2", A, B, OperationType.REQRES));
		i2.addOperation(new Operation("MyOneWay2", Type.INT(), null, OperationType.ONEWAY));
		interfacesInputPort2.add(i2);
		InputPort InputPort2 = new InputPort("InputPort2", "socket://localhost:8082", "http", interfacesInputPort2);

		target.put(SymbolTable.newPair("InputPort2", SymbolType.INPUT_PORT), InputPort2);
		
		return AppTest.testSymbolsForEquality(result, target);
	}
}
