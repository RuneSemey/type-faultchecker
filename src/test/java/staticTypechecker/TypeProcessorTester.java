package staticTypechecker;

import java.util.List;

import staticTypechecker.utils.ModuleHandler;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.entities.SymbolTable;
import staticTypechecker.entities.ChoiceType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.Type;
import staticTypechecker.entities.Module;;

public class TypeProcessorTester {
	public static boolean run(){
		String moduleName = AppTest.BASE_PATH + "testFilesForTypeProcessor/main.ol";
		List<Module> modules = ModuleHandler.loadModule(moduleName);
		AppTest.runProcessors(modules, 1);

		SymbolTable result = ModuleHandler.get(moduleName).symbols();
		SymbolTable target = new SymbolTable();

		// add all the types manually

		// type importedType1
		InlineType importedType1 = Type.INT();
		importedType1.addChildUnsafe("x", Type.INT());
		importedType1.addChildUnsafe("y", Type.INT());
		target.put(SymbolTable.newPair("importedType1", SymbolType.TYPE), importedType1);

		// type importedType2
		InlineType importedType2 = Type.STRING();
		ChoiceType importedC = new ChoiceType();
		importedC.addChoiceUnsafe(Type.STRING());
		importedC.addChoiceUnsafe(Type.INT());
		importedType2.addChildUnsafe("x", importedC);
		target.put(SymbolTable.newPair("importedType2", SymbolType.TYPE), importedType2);

		// type importedCircular
		InlineType importedCircular = Type.VOID();
		InlineType importedK = Type.INT();
		importedK.addChildUnsafe("x", importedCircular);
		importedCircular.addChildUnsafe("x", importedK);
		target.put(SymbolTable.newPair("importedCircular", SymbolType.TYPE), importedCircular);

		// type A
		InlineType A = Type.VOID();
		A.addChildUnsafe("x", Type.INT());
		A.addChildUnsafe("y", Type.STRING().addChild("z", Type.BOOL()));
		target.put(SymbolTable.newPair("A", SymbolType.TYPE), A);

		// type B
		InlineType B = Type.ANY().setOpenStatus(true);
		B.addChildUnsafe("x", Type.INT());
		target.put(SymbolTable.newPair("B", SymbolType.TYPE), B);

		// type C
		ChoiceType C = new ChoiceType();
		C.addChoiceUnsafe(Type.STRING());
		C.addChoiceUnsafe(Type.INT());
		target.put(SymbolTable.newPair("C", SymbolType.TYPE), C);

		// type D
		ChoiceType D = new ChoiceType();
		D.addChoiceUnsafe(A);
		D.addChoiceUnsafe(C);
		target.put(SymbolTable.newPair("D", SymbolType.TYPE), D);

		// type E
		InlineType E = Type.INT();
		E.addChildUnsafe("x", A);
		target.put(SymbolTable.newPair("E", SymbolType.TYPE), E);

		// type F
		InlineType F = Type.STRING();
		F.addChildUnsafe("x", F);
		target.put(SymbolTable.newPair("F", SymbolType.TYPE), F);

		// type G and H
		InlineType G = Type.STRING();
		InlineType H = Type.INT();

		G.addChildUnsafe("x", H);
		H.addChildUnsafe("x", G);

		target.put(SymbolTable.newPair("G", SymbolType.TYPE), G);
		target.put(SymbolTable.newPair("H", SymbolType.TYPE), H);

		// type I
		InlineType I = Type.BOOL();
		ChoiceType IChildX = new ChoiceType();
		IChildX.addChoiceUnsafe(I);
		IChildX.addChoiceUnsafe(Type.VOID());
		I.addChildUnsafe("x", IChildX);

		target.put(SymbolTable.newPair("I", SymbolType.TYPE), I);

		// type J
		InlineType J = Type.INT();
		J.addChildUnsafe("x", Type.STRING().addChild("y", J));
		target.put(SymbolTable.newPair("J", SymbolType.TYPE), J);

		// type K
		InlineType K = Type.INT().addChild("x", importedCircular);		
		target.put(SymbolTable.newPair("K", SymbolType.TYPE), K);

		// type Weird
		InlineType weird = Type.INT();
		ChoiceType weirdChild = new ChoiceType();
		weird.addChildUnsafe("y", weirdChild);
		weirdChild.addChoiceUnsafe(weird);
		weirdChild.addChoiceUnsafe(Type.INT());
		target.put(SymbolTable.newPair("Weird", SymbolType.TYPE), weird);

		return AppTest.testSymbolsForEquality(result, target);
	}
}
