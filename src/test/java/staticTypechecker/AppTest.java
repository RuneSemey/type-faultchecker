package staticTypechecker;

import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Map.Entry;

import org.junit.Test;

import staticTypechecker.entities.SymbolTable.Pair;
import staticTypechecker.entities.Module;
import staticTypechecker.entities.Symbol;
import staticTypechecker.entities.SymbolTable;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.visitors.InputPortProcessor;
import staticTypechecker.visitors.InterfaceProcessor;
import staticTypechecker.visitors.OutputPortProcessor;
import staticTypechecker.visitors.SymbolCollector;
import staticTypechecker.visitors.Synthesizer;
import staticTypechecker.visitors.TypeCheckerVisitor;
import staticTypechecker.visitors.TypeProcessor;

/**
 * Unit test for simple App.
 */
public class AppTest{
	public static final String BASE_PATH = "./src/test/files/";

    /**
     * Test the symbol checker
     */
    @Test
    public void testSymbolChecking(){
		assertTrue(SymbolCollectorTester.run());
    }

	@Test
	public void testTypeProcessor(){
		assertTrue(TypeProcessorTester.run());
	}

	@Test
	public void testInterfaceProcessor(){
		assertTrue(InterfaceProcessorTester.run());
	}

	@Test
	public void testInputPortProcessor(){
		assertTrue(InputPortProcessorTester.run());
	}

	@Test
	public void testOutputPortProcessor(){
		assertTrue(OutputPortProcessorTester.run());
	}

	@Test
	public void testNil(){
		assertTrue(BehaviourProcessorTester.testNil());
	}

	@Test
	public void testSeq(){
		assertTrue(BehaviourProcessorTester.testSeq());
	}

	@Test
	public void testNotify(){
		assertTrue(BehaviourProcessorTester.testNotify());
	}

	@Test
	public void testOneWay(){
		assertTrue(BehaviourProcessorTester.testOneWay());
	}

	@Test
	public void testSolicit(){
		assertTrue(BehaviourProcessorTester.testSolicit());
	}
	
	@Test
	public void testRequest(){
		assertTrue(BehaviourProcessorTester.testRequest());
	}
	
	@Test
	public void testChoice(){
		assertTrue(BehaviourProcessorTester.testChoice());
	}

	@Test
	public void testWhile(){
		assertTrue(BehaviourProcessorTester.testWhile());
	}

	@Test
	public void testWhileFallback1(){
		assertTrue(BehaviourProcessorTester.testWhileFallback1());
	}

	@Test
	public void testWhileFallback2(){
		assertTrue(BehaviourProcessorTester.testWhileFallback2());
	}
	
	@Test
	public void testNestedWhile(){
		assertTrue(BehaviourProcessorTester.testNestedWhile());
	}

	@Test
	public void testNestedWhileFallback(){
		assertTrue(BehaviourProcessorTester.testNestedWhileFallback());
	}

	@Test
	public void testNestedWhileDoubleFallback(){
		assertTrue(BehaviourProcessorTester.testNestedWhileDoubleFallback());
	}

	@Test
	public void testIf(){
		assertTrue(BehaviourProcessorTester.testIf());
	}

	@Test
	public void testNestedIf(){
		assertTrue(BehaviourProcessorTester.testNestedIf());
	}

	@Test
	public void testAssign(){
		assertTrue(BehaviourProcessorTester.testAssign());
	}

	@Test
	public void testUndef(){
		assertTrue(BehaviourProcessorTester.testUndef());
	}

	@Test
	public void testDeepCopy(){
		assertTrue(BehaviourProcessorTester.testDeepCopy());
	}

	@Test
	public void testOperationAssign(){
		assertTrue(BehaviourProcessorTester.testOperationAssign());
	}

	@Test
	public void testTypeCasting(){
		assertTrue(BehaviourProcessorTester.testTypeCasting());
	}

	@Test
	public void testErrors(){
		assertTrue(BehaviourProcessorTester.testErrors());
	}

	@Test
	public void testRecursiveAssign(){
		assertTrue(BehaviourProcessorTester.testRecursiveAssign());
	}

	@Test
	public void testWhileTypeHint(){
		System.setProperty("typehint", "assertions");
		assertTrue(BehaviourProcessorTester.testWhileTypeHint());
	}

	/**
	 * Runs the processors in order up to and including the one specified in parameter steps on the given modules.
	 * Step 0: symbolcollector,
	 * Step 1: type processor,
	 * Step 2: interface processor,
	 * Step 3: input port processor,
	 * Step 4: output port processor,
	 * Step 5: synthesizer
	 */
	public static void runProcessors(List<Module> modules, int steps){
		TypeCheckerVisitor[] visitors = {
			new SymbolCollector(),
			new TypeProcessor(),
			new InterfaceProcessor(),
			new InputPortProcessor(),
			new OutputPortProcessor()
		};

		for(int i = 0; i <= Math.min(steps, 4); i++){
			for(Module m : modules){
				visitors[i].process(m, false);
			}

			for(Module m : modules){
				visitors[i].process(m, true);
			}
		}

		if(steps == 5){
			for(Module m : modules){
				Synthesizer.get(m).synthesize();
			}
		}
	}

	public static boolean testSymbolsForEquality(SymbolTable result, SymbolTable target){
		for(Entry<Pair<String, SymbolType>, Symbol> ent : target.entrySet()){
			String symbolName = ent.getKey().key();
			SymbolType symbolType = ent.getKey().value();
			Symbol targetSymbol = ent.getValue();
			Symbol resultSymbol = result.get(symbolName, symbolType);

			if(targetSymbol == null || resultSymbol == null){
				System.out.println("One is null for symbol: " + symbolName + "\n  Result: " + resultSymbol + "\n  Target: " + targetSymbol);
				return false;
			}

			if(!Symbol.equals(targetSymbol, resultSymbol)){
				System.out.println("FAIL on symbol " + symbolName + ":\nresult:\n" + resultSymbol.prettyString() + "\n\nis not equal to expected:\n" + targetSymbol.prettyString());
				return false;
			}

		}
		
		return true;
	}
}
