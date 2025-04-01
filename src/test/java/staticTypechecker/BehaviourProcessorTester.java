package staticTypechecker;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import jolie.lang.parse.ast.EmbedServiceNode;
import jolie.lang.parse.ast.OLSyntaxNode;
import jolie.lang.parse.ast.ServiceNode;
import staticTypechecker.utils.ModuleHandler;
import staticTypechecker.visitors.Synthesizer;
import staticTypechecker.entities.ChoiceType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.Module;
import staticTypechecker.entities.Service;
import staticTypechecker.entities.Type;
import staticTypechecker.faults.Fault;
import staticTypechecker.faults.FaultHandler;
import staticTypechecker.faults.NoServiceParameterFault;
import staticTypechecker.faults.PortsIncompatibleFault;
import staticTypechecker.faults.TypeFault;
import staticTypechecker.faults.UnknownFunctionFault;

public class BehaviourProcessorTester {
	public static boolean testNil(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testNil.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();
		Type target = Type.VOID();
		
		return result.equals(target);
	}

	public static boolean testSeq(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testSeq.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();
		InlineType a = Type.INT();
		InlineType b = Type.STRING();

		target.addChildUnsafe("a", a);
		target.addChildUnsafe("b", b);

		return result.equals(target);
	}

	public static boolean testNotify(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testNotify.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();
		InlineType inputType = Type.INT();
		inputType.addChildUnsafe("x", Type.STRING());
		inputType.addChildUnsafe("y", Type.INT());
		target.addChildUnsafe("inputType", inputType);

		return result.equals(target);
	}

	public static boolean testOneWay(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testOneWay.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();
		InlineType inputType = Type.INT();
		InlineType p = Type.INT();

		inputType.addChildUnsafe("x", Type.STRING());
		inputType.addChildUnsafe("y", Type.INT());
		p.addChildUnsafe("x", Type.STRING());
		p.addChildUnsafe("y", Type.INT());

		target.addChildUnsafe("inputType", inputType);
		target.addChildUnsafe("p", p);

		return result.equals(target);
	}

	public static boolean testSolicit(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testSolicit.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();

		// arg
		InlineType arg = Type.INT().addChild("x", Type.STRING()).addChild("y", Type.INT());
		target.addChildUnsafe("arg", arg);

		// out
		ChoiceType child = new ChoiceType();
		child.addChoiceUnsafe(Type.STRING());
		child.addChoiceUnsafe(Type.INT());

		InlineType out = Type.STRING().addChild("x", child);
		target.addChildUnsafe("out", out);

		return result.equals(target);
	}

	public static boolean testRequest(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testRequest.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();

		// arg
		InlineType arg = Type.INT().addChild("x", Type.STRING()).addChild("y", Type.INT());
		target.addChildUnsafe("arg", arg);

		// f
		InlineType f = Type.INT();
		target.addChildUnsafe("f", f);

		// out
		InlineType out = Type.STRING().addChild("x", Type.INT());
		target.addChildUnsafe("out", out);

		return result.equals(target);
	}

	public static boolean testChoice(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testChoice.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();
		
		// first choice, tree1
		InlineType tree1 = Type.VOID();
		tree1.addChildUnsafe("arg", Type.INT().addChild("x", Type.STRING()).addChild("y", Type.INT()));
		tree1.addChildUnsafe("out", Type.STRING().addChild("x", Type.STRING()).addChild("y", Type.INT()));

		// second choice, tree2
		InlineType tree2 = Type.VOID();
		tree2.addChildUnsafe("input", Type.INT());
		tree2.addChildUnsafe("out", Type.INT());

		// third choice, tree3
		InlineType tree3 = Type.VOID();
		tree3.addChildUnsafe("input", Type.STRING());
		tree3.addChildUnsafe("out", Type.BOOL());
		tree3.addChildUnsafe("random", Type.INT());

		target.addChoiceUnsafe(tree1);
		target.addChoiceUnsafe(tree2);
		target.addChoiceUnsafe(tree3);

		return result.equals(target);
	}

	public static boolean testWhile(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testWhile.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, not entering the while loop
		InlineType option1 = Type.VOID().addChild("a", Type.INT()).addChild("i", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, 1st iteration of the while loop, not entering the if statement
		InlineType option2 = Type.VOID().addChild("a", Type.INT()).addChild("b", Type.STRING()).addChild("i", Type.INT());
		target.addChoiceUnsafe(option2);

		// third option, 1st iteration of the while loop, entering the if statement
		InlineType option3 = Type.VOID().addChild("a", Type.BOOL()).addChild("b", Type.STRING()).addChild("c", Type.BOOL()).addChild("i", Type.INT());
		target.addChoiceUnsafe(option3);

		// fourth option, 2nd iteration of the while loop, not entering the if statement
		InlineType option4 = Type.VOID().addChild("a", Type.INT()).addChild("b", Type.STRING()).addChild("c", Type.BOOL()).addChild("i", Type.INT());
		target.addChoiceUnsafe(option4);

		return result.equals(target);
	}

	public static boolean testWhileFallback1(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testWhileFallback1.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, never entered the while loop
		InlineType option1 = Type.VOID().addChild("a", Type.INT()).addChild("i", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, enter the while loop, but fail to find a steady state, so fallback plan
		InlineType option2 = Type.VOID().addChild("a", Type.INT().addChild("b", Type.UNDEFINED())).addChild("i", Type.INT());
		target.addChoiceUnsafe(option2);

		return result.equals(target);
	}

	public static boolean testWhileFallback2(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testWhileFallback2.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, not in if statement, and not entered the while loop
		InlineType option1 = Type.VOID().addChild("a", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, in if statement, and not entered the while loop
		InlineType option2 = Type.VOID().addChild("a", Type.STRING());
		target.addChoiceUnsafe(option2);

		// enter the while loop, but fail to find a steady state, so fallback plan
		// first choice of fallback
		InlineType fallback1 = Type.VOID().addChild("a", Type.INT().addChild("b", Type.UNDEFINED()).addChild("c", Type.UNDEFINED()));
		target.addChoiceUnsafe(fallback1);

		// second choice of fallback
		InlineType fallback2 = Type.VOID().addChild("a", Type.STRING().addChild("b", Type.UNDEFINED()).addChild("c", Type.UNDEFINED()));

		target.addChoiceUnsafe(fallback2);

		return result.equals(target);
	}

	public static boolean testNestedWhile(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testNestedWhile.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, not entering the first while loop
		InlineType option1 = Type.VOID().addChild("a", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, enter first while, but not second while
		InlineType option2 = Type.VOID().addChild("a", Type.STRING()).addChild("b", Type.STRING());
		target.addChoiceUnsafe(option2);

		// third option, enter both whiles
		InlineType option3 = Type.VOID().addChild("a", Type.STRING()).addChild("b", Type.STRING()).addChild("c", Type.BOOL());
		target.addChoiceUnsafe(option3);

		return result.equals(target);
	}

	public static boolean testNestedWhileFallback(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testNestedWhileFallback.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, not entering the first while loop
		InlineType option1 = Type.VOID().addChild("a", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, enter first while, but not second while
		InlineType option2 = Type.VOID().addChild("a", Type.STRING()).addChild("b", Type.STRING()).addChild("c", Type.VOID());
		target.addChoiceUnsafe(option2);

		// third option, enter both whiles
		InlineType option3 = Type.VOID();
		option3.addChildUnsafe("a", Type.STRING());
		option3.addChildUnsafe("b", Type.STRING().addChild("x", Type.UNDEFINED()));
		option3.addChildUnsafe("c", Type.UNDEFINED());
		target.addChoiceUnsafe(option3);

		return result.equals(target);
	}

	public static boolean testNestedWhileDoubleFallback(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testNestedWhileDoubleFallback.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, not entering the first while loop
		InlineType option1 = Type.VOID().addChild("a", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, both while loops fail and do fallback
		InlineType option2 = Type.VOID();
		option2.addChildUnsafe("a", Type.UNDEFINED().addChild("x", Type.UNDEFINED()));
		option2.addChildUnsafe("b", Type.UNDEFINED().addChild("x", Type.UNDEFINED()));
		target.addChoiceUnsafe(option2);

		return result.equals(target);
	}

	public static boolean testIf(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testIf.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, not entering the if
		InlineType option1 = Type.VOID();
		option1.addChildUnsafe("a", Type.INT());
		option1.addChildUnsafe("b", Type.STRING().addChild("x", Type.BOOL()));
		option1.addChildUnsafe("d", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, entering the if
		InlineType option2 = Type.VOID();
		option2.addChildUnsafe("a", Type.INT().addChild("x", Type.BOOL()));
		option2.addChildUnsafe("b", Type.INT().addChild("x", Type.BOOL()));
		option2.addChildUnsafe("c", Type.STRING());
		option2.addChildUnsafe("d", Type.INT());
		target.addChoiceUnsafe(option2);

		return result.equals(target);
	}

	public static boolean testNestedIf(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testNestedIf.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// first option, not any if
		InlineType option1 = Type.VOID();
		option1.addChildUnsafe("a", Type.INT());
		option1.addChildUnsafe("b", Type.STRING().addChild("x", Type.BOOL()));
		option1.addChildUnsafe("d", Type.INT());
		target.addChoiceUnsafe(option1);

		// second option, entering the first if, but not the second
		InlineType option2 = Type.VOID();
		option2.addChildUnsafe("a", Type.INT().addChild("x", Type.BOOL()));
		option2.addChildUnsafe("b", Type.INT().addChild("x", Type.BOOL()));
		option2.addChildUnsafe("c", Type.STRING());
		option2.addChildUnsafe("d", Type.INT());
		target.addChoiceUnsafe(option2);

		// third option, entering both ifs
		InlineType option3 = Type.VOID();
		option3.addChildUnsafe("a", Type.DOUBLE().addChild("x", Type.BOOL()));
		option3.addChildUnsafe("b", Type.INT().addChild("x", Type.BOOL().addChild("y", Type.STRING())));
		option3.addChildUnsafe("c", Type.STRING().addChild("x", Type.INT()));
		option3.addChildUnsafe("d", Type.INT());
		option3.addChildUnsafe("e", Type.INT());
		target.addChoiceUnsafe(option3);

		return result.equals(target);
	}

	public static boolean testAssign(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testAssign.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();

		target.addChildUnsafe("a", Type.LONG().addChild("x", Type.STRING().addChild("y", Type.BOOL())));
		target.addChildUnsafe("b", Type.STRING().addChild("h", Type.LONG()));
		target.addChildUnsafe("c", Type.BOOL());

		return result.equals(target);
	}

	public static boolean testUndef(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testUndef.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID().addChild("b", Type.STRING());

		return result.equals(target);
	}

	public static boolean testDeepCopy(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testDeepCopy.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();

		// a
		InlineType a = Type.INT();
		a.addChildUnsafe("x", Type.STRING().addChild("z", Type.BOOL()));
		a.addChildUnsafe("y", Type.INT());
		target.addChildUnsafe("a", a);

		// b
		target.addChildUnsafe("b", a);

		// c
		target.addChildUnsafe("c", a);

		// d
		InlineType d = Type.STRING().addChild("z", Type.BOOL());
		target.addChildUnsafe("d", d);

		return result.equals(target);
	}

	public static boolean testOperationAssign(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testOperationAssign.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		InlineType result = (InlineType)Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();

		// -------------- bools -------------- 
		// plus
		target.addChildUnsafe("bp1", Type.BOOL());
		target.addChildUnsafe("bp2", Type.INT());
		target.addChildUnsafe("bp3", Type.LONG());
		target.addChildUnsafe("bp4", Type.DOUBLE());
		target.addChildUnsafe("bp5", Type.STRING());
		target.addChildUnsafe("bp6", Type.STRING());

		// minus
		target.addChildUnsafe("bmi1", Type.BOOL());
		target.addChildUnsafe("bmi2", Type.INT());
		target.addChildUnsafe("bmi3", Type.LONG());
		target.addChildUnsafe("bmi4", Type.DOUBLE());
		target.addChildUnsafe("bmi5", Type.BOOL());
		target.addChildUnsafe("bmi6", Type.BOOL());

		// multiply
		target.addChildUnsafe("bmu1", Type.BOOL());
		target.addChildUnsafe("bmu2", Type.INT());
		target.addChildUnsafe("bmu3", Type.LONG());
		target.addChildUnsafe("bmu4", Type.DOUBLE());
		target.addChildUnsafe("bmu5", Type.BOOL());
		target.addChildUnsafe("bmu6", Type.BOOL());

		// divide
		target.addChildUnsafe("bd1", Type.BOOL());
		target.addChildUnsafe("bd2", Type.INT());
		target.addChildUnsafe("bd3", Type.LONG());
		target.addChildUnsafe("bd4", Type.DOUBLE());
		target.addChildUnsafe("bd5", Type.BOOL());
		target.addChildUnsafe("bd6", Type.BOOL());

		// modulo
		target.addChildUnsafe("bmo1", Type.BOOL());
		target.addChildUnsafe("bmo2", Type.INT());
		target.addChildUnsafe("bmo3", Type.LONG());
		target.addChildUnsafe("bmo4", Type.DOUBLE());
		target.addChildUnsafe("bmo5", Type.BOOL());
		target.addChildUnsafe("bmo6", Type.BOOL());

		// -------------- ints -------------- 
		// plus
		target.addChildUnsafe("ip1", Type.INT());
		target.addChildUnsafe("ip2", Type.INT());
		target.addChildUnsafe("ip3", Type.LONG());
		target.addChildUnsafe("ip4", Type.DOUBLE());
		target.addChildUnsafe("ip5", Type.STRING());
		target.addChildUnsafe("ip6", Type.STRING());

		// minus
		target.addChildUnsafe("imi1", Type.INT());
		target.addChildUnsafe("imi2", Type.INT());
		target.addChildUnsafe("imi3", Type.LONG());
		target.addChildUnsafe("imi4", Type.DOUBLE());
		target.addChildUnsafe("imi5", Type.INT());
		target.addChildUnsafe("imi6", Type.INT());

		// multiply
		target.addChildUnsafe("imu1", Type.INT());
		target.addChildUnsafe("imu2", Type.INT());
		target.addChildUnsafe("imu3", Type.LONG());
		target.addChildUnsafe("imu4", Type.DOUBLE());
		target.addChildUnsafe("imu5", Type.INT());
		target.addChildUnsafe("imu6", Type.INT());

		// divide
		target.addChildUnsafe("id1", Type.INT());
		target.addChildUnsafe("id2", Type.INT());
		target.addChildUnsafe("id3", Type.LONG());
		target.addChildUnsafe("id4", Type.DOUBLE());
		target.addChildUnsafe("id5", Type.INT());
		target.addChildUnsafe("id6", Type.INT());

		// modulo
		target.addChildUnsafe("imo1", Type.INT());
		target.addChildUnsafe("imo2", Type.INT());
		target.addChildUnsafe("imo3", Type.LONG());
		target.addChildUnsafe("imo4", Type.DOUBLE());
		target.addChildUnsafe("imo5", Type.INT());
		target.addChildUnsafe("imo6", Type.INT());

		// -------------- longs -------------- 
		// plus
		target.addChildUnsafe("lp1", Type.LONG());
		target.addChildUnsafe("lp2", Type.LONG());
		target.addChildUnsafe("lp3", Type.LONG());
		target.addChildUnsafe("lp4", Type.DOUBLE());
		target.addChildUnsafe("lp5", Type.STRING());
		target.addChildUnsafe("lp6", Type.STRING());

		// minus
		target.addChildUnsafe("lmi1", Type.LONG());
		target.addChildUnsafe("lmi2", Type.LONG());
		target.addChildUnsafe("lmi3", Type.LONG());
		target.addChildUnsafe("lmi4", Type.DOUBLE());
		target.addChildUnsafe("lmi5", Type.LONG());
		target.addChildUnsafe("lmi6", Type.LONG());

		// multiply
		target.addChildUnsafe("lmu1", Type.LONG());
		target.addChildUnsafe("lmu2", Type.LONG());
		target.addChildUnsafe("lmu3", Type.LONG());
		target.addChildUnsafe("lmu4", Type.DOUBLE());
		target.addChildUnsafe("lmu5", Type.LONG());
		target.addChildUnsafe("lmu6", Type.LONG());

		// divide
		target.addChildUnsafe("ld1", Type.LONG());
		target.addChildUnsafe("ld2", Type.LONG());
		target.addChildUnsafe("ld3", Type.LONG());
		target.addChildUnsafe("ld4", Type.DOUBLE());
		target.addChildUnsafe("ld5", Type.LONG());
		target.addChildUnsafe("ld6", Type.LONG());

		// modulo
		target.addChildUnsafe("lmo1", Type.LONG());
		target.addChildUnsafe("lmo2", Type.LONG());
		target.addChildUnsafe("lmo3", Type.LONG());
		target.addChildUnsafe("lmo4", Type.DOUBLE());
		target.addChildUnsafe("lmo5", Type.LONG());
		target.addChildUnsafe("lmo6", Type.LONG());

		// -------------- doubles -------------- 
		// plus
		target.addChildUnsafe("dp1", Type.DOUBLE());
		target.addChildUnsafe("dp2", Type.DOUBLE());
		target.addChildUnsafe("dp3", Type.DOUBLE());
		target.addChildUnsafe("dp4", Type.DOUBLE());
		target.addChildUnsafe("dp5", Type.STRING());
		target.addChildUnsafe("dp6", Type.STRING());

		// minus
		target.addChildUnsafe("dmi1", Type.DOUBLE());
		target.addChildUnsafe("dmi2", Type.DOUBLE());
		target.addChildUnsafe("dmi3", Type.DOUBLE());
		target.addChildUnsafe("dmi4", Type.DOUBLE());
		target.addChildUnsafe("dmi5", Type.DOUBLE());
		target.addChildUnsafe("dmi6", Type.DOUBLE());

		// multiply
		target.addChildUnsafe("dmu1", Type.DOUBLE());
		target.addChildUnsafe("dmu2", Type.DOUBLE());
		target.addChildUnsafe("dmu3", Type.DOUBLE());
		target.addChildUnsafe("dmu4", Type.DOUBLE());
		target.addChildUnsafe("dmu5", Type.DOUBLE());
		target.addChildUnsafe("dmu6", Type.DOUBLE());

		// divide
		target.addChildUnsafe("dd1", Type.DOUBLE());
		target.addChildUnsafe("dd2", Type.DOUBLE());
		target.addChildUnsafe("dd3", Type.DOUBLE());
		target.addChildUnsafe("dd4", Type.DOUBLE());
		target.addChildUnsafe("dd5", Type.DOUBLE());
		target.addChildUnsafe("dd6", Type.DOUBLE());

		// modulo
		target.addChildUnsafe("dmo1", Type.DOUBLE());
		target.addChildUnsafe("dmo2", Type.DOUBLE());
		target.addChildUnsafe("dmo3", Type.DOUBLE());
		target.addChildUnsafe("dmo4", Type.DOUBLE());
		target.addChildUnsafe("dmo5", Type.DOUBLE());
		target.addChildUnsafe("dmo6", Type.DOUBLE());

		// -------------- strings -------------- 
		// plus
		target.addChildUnsafe("sp1", Type.STRING());
		target.addChildUnsafe("sp2", Type.STRING());
		target.addChildUnsafe("sp3", Type.STRING());
		target.addChildUnsafe("sp4", Type.STRING());
		target.addChildUnsafe("sp5", Type.STRING());
		target.addChildUnsafe("sp6", Type.STRING());

		// minus
		target.addChildUnsafe("smi1", Type.STRING());
		target.addChildUnsafe("smi2", Type.INT());
		target.addChildUnsafe("smi3", Type.LONG());
		target.addChildUnsafe("smi4", Type.DOUBLE());
		target.addChildUnsafe("smi5", Type.STRING());
		target.addChildUnsafe("smi6", Type.STRING());

		// multiply
		target.addChildUnsafe("smu1", Type.BOOL());
		target.addChildUnsafe("smu2", Type.INT());
		target.addChildUnsafe("smu3", Type.LONG());
		target.addChildUnsafe("smu4", Type.DOUBLE());
		target.addChildUnsafe("smu5", Type.STRING());
		target.addChildUnsafe("smu6", Type.STRING());

		// divide
		target.addChildUnsafe("sd1", Type.STRING());
		target.addChildUnsafe("sd2", Type.INT());
		target.addChildUnsafe("sd3", Type.LONG());
		target.addChildUnsafe("sd4", Type.DOUBLE());
		target.addChildUnsafe("sd5", Type.STRING());
		target.addChildUnsafe("sd6", Type.STRING());

		// modulo
		target.addChildUnsafe("smo1", Type.STRING());
		target.addChildUnsafe("smo2", Type.INT());
		target.addChildUnsafe("smo3", Type.LONG());
		target.addChildUnsafe("smo4", Type.DOUBLE());
		target.addChildUnsafe("smo5", Type.STRING());
		target.addChildUnsafe("smo6", Type.STRING());


		for(Entry<String, Type> tarEnt : target.children().entrySet()){
			String name = tarEnt.getKey();
			Type tarChild = tarEnt.getValue();
			Type resChild = result.getChild(name);

			if(!tarChild.equals(resChild)){
				System.out.println("Conflict on " + name + ":\nresChild: " + resChild.prettyString() + " not equal to tarChild: " + tarChild.prettyString());
				return false;
			}
		}

		return true;
	}

	public static boolean testTypeCasting(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testTypeCasting.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		InlineType target = Type.VOID();
		
		target.addChildUnsafe("a", Type.INT());
		target.addChildUnsafe("b", Type.LONG());
		target.addChildUnsafe("c", Type.DOUBLE());
		target.addChildUnsafe("d", Type.STRING());
		target.addChildUnsafe("e", Type.BOOL());

		return result.equals(target);
	}
	
	public static boolean testErrors(){
		String[] moduleNames = {
			"embedInterfaceError.ol",
			"ifError.ol",
			"notifyError.ol",
			"paramError.ol",
			"reqresError.ol",
			"solicitError.ol",
			"unknownFunctionError.ol"
		};

		ArrayList<Module> modules = new ArrayList<>();
		for(String moduleName : moduleNames){
			modules.addAll(ModuleHandler.loadModule(AppTest.BASE_PATH + "testFilesForErrors/" + moduleName));
		}
		AppTest.runProcessors(modules, 5);

		PortsIncompatibleFault embedFault = new PortsIncompatibleFault("Critical error in file '/mnt/c/Users/Kasper/Desktop/sdu/speciale/staticTypeChecker-jolie/./src/test/files/testFilesForErrors/embedInterfaceError.ol' on line 44:\nservice 'EmbedMe' does not provide operations:\n\tmyOneWay(void)\nrequired by port 'mainPort'");

		TypeFault ifFault = new TypeFault("Critical error in file '/mnt/c/Users/Kasper/Desktop/sdu/speciale/staticTypeChecker-jolie/./src/test/files/testFilesForErrors/ifError.ol' on line 6:\nGuard of if-statement is not subtype of bool { ? }. Found type:\nint {\n\tx: string\n}", null);

		TypeFault notifyFault = new TypeFault("Critical error in file '/mnt/c/Users/Kasper/Desktop/sdu/speciale/staticTypeChecker-jolie/./src/test/files/testFilesForErrors/notifyError.ol' on line 21:\nType given to \"sendMessage\" is different from what is expected. Given type:\nvoid {\n\tmessage: string\n\tsender: string\n}\n\nExpected type:\nvoid {\n\tsender: string\n\ttime: int\n\tmessage: string\n}", null);

		TypeFault reqresFault = new TypeFault("Critical error in file '/mnt/c/Users/Kasper/Desktop/sdu/speciale/staticTypeChecker-jolie/./src/test/files/testFilesForErrors/reqresError.ol' on line 20:\noperation \"sendMessage\" does not have the expected return type.\nActual return type:\nint\n\nExpected return type:\nbool", null);

		TypeFault solicitError = new TypeFault("Critical error in file '/mnt/c/Users/Kasper/Desktop/sdu/speciale/staticTypeChecker-jolie/./src/test/files/testFilesForErrors/notifyError.ol' on line 21:\nType given to \"sendMessage\" is different from what is expected. Given type:\nvoid {\n\tmessage: string\n\tsender: string\n}\n\nExpected type:\nvoid {\n\tsender: string\n\ttime: int\n\tmessage: string\n}", null);

		Service embedMe = new Service();
		embedMe.setName("EmbedMe");
		embedMe.setParameter(Type.INT());
		NoServiceParameterFault noParamError = null;

		// find the parsing context of the embedding to use in the fault
		for(Module m : modules){
			if(m.name().equals("paramError.ol")){
				for(OLSyntaxNode n : m.program().children()){
					if(n instanceof ServiceNode && ((ServiceNode)n).name().equals("Main")){
						ServiceNode s = (ServiceNode)n;
						for(OLSyntaxNode c : s.program().children()){
							if(c instanceof EmbedServiceNode && ((EmbedServiceNode)c).bindingPort().id().equals("lol")){
								System.out.println();
								noParamError = new NoServiceParameterFault(embedMe, c.context());
							}
						}
					}
				}
			}
		}

		TypeFault wrongParamError = new TypeFault("Critical error in file '/mnt/c/Users/Kasper/Desktop/sdu/speciale/staticTypeChecker-jolie/./src/test/files/testFilesForErrors/paramError.ol' on line 6:\nType given to service: \"EmbedMe\" is not of expected type. Type given:\nstring\n\nType expected:\nint", null);;

		UnknownFunctionFault unknownFuncError = new UnknownFunctionFault("Critical error in file '/mnt/c/Users/Kasper/Desktop/sdu/speciale/staticTypeChecker-jolie/./src/test/files/testFilesForErrors/unknownFunctionError.ol' on line 7:\nThe operation 'reqResFunction' is unknown in service 'MyService'. Maybe you forgot to give the service an inputPort with an interface which provides the operation?", null);

		Fault[] faults = {
			ifFault,
			notifyFault,
			reqresFault,
			solicitError,
			noParamError,
			wrongParamError,
			unknownFuncError,
			embedFault
		};

		for(Fault f : faults){
			if(!FaultHandler.contains(f)){
				System.out.println("fault is not present:\n" + f.getMessage());
				return false;
			}
		}

		return true;
	}

	public static boolean testWhileTypeHint(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testWhileTypeHint.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();

		ChoiceType target = new ChoiceType();

		// tree 1, original tree before while loop
		InlineType t1 = Type.VOID().addChild("a", Type.INT());

		// tree 2, the steady state of the while loop
		InlineType t2 = Type.VOID();
		ChoiceType a = new ChoiceType();
		a.addChoiceUnsafe(Type.INT());
		
		InlineType recInt = Type.INT();
		recInt.addChildUnsafe("x", a);
		a.addChoiceUnsafe(recInt);
		a.addChoiceUnsafe(Type.INT());
		t2.addChildUnsafe("a", a);

		target.addChoiceUnsafe(t1);
		target.addChoiceUnsafe(t2);

		return result.equals(target);
	}

	public static boolean testRecursiveAssign(){
		String moduleName = AppTest.BASE_PATH + "testFilesForBehaviours/testRecursiveAssign.ol";
		List<Module> modules = BehaviourProcessorTester.readyModules(moduleName);

		Type result = Synthesizer.get(modules.get(0)).synthesize().getType();
		InlineType target = Type.VOID();

		// parameter with the recursive children
		InlineType p = Type.VOID();

		// r1
		InlineType r1 = Type.INT();
		r1.addChildUnsafe("x", r1);

		InlineType r1Copy = r1.copy();

		r1.addChildUnsafe("x", Type.INT().addChild("x", Type.STRING().addChild("x", r1Copy)));
		p.addChildUnsafe("r1", r1);

		// r2
		InlineType r2 = Type.INT();
		r2.addChildUnsafe("y", r2);
		ChoiceType r2X = new ChoiceType();
		r2X.addChoiceUnsafe(Type.BOOL().addChild("r", r2X));
		r2X.addChoiceUnsafe(Type.STRING().addChild("z", Type.STRING()));
		r2.addChildUnsafe("x", r2X);

		InlineType r2Copy = r2.copy();

		r2.addChildUnsafe("y", r2Copy);
		ChoiceType newR2X = new ChoiceType();
		newR2X.addChoiceUnsafe(Type.BOOL().addChild("r", r2Copy.getChild("x")).addChild("z", Type.INT()));
		newR2X.addChoiceUnsafe(Type.STRING().addChild("z", Type.INT()));
		r2.addChildUnsafe("x", newR2X);
		p.addChildUnsafe("r2", r2);

		target.addChildUnsafe("p", p);

		return result.equals(target);
	}

	private static List<Module> readyModules(String moduleName){
		List<Module> modules = ModuleHandler.loadModule(moduleName);
		AppTest.runProcessors(modules, 4);
		return modules;
	}
}
