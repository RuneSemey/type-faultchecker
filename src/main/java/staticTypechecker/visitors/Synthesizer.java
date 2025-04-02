package staticTypechecker.visitors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;
import jolie.lang.Constants.OperandType;
import jolie.lang.parse.OLVisitor;
import jolie.lang.parse.ast.AddAssignStatement;
import jolie.lang.parse.ast.AssignStatement;
import jolie.lang.parse.ast.CompareConditionNode;
import jolie.lang.parse.ast.CompensateStatement;
import jolie.lang.parse.ast.CorrelationSetInfo;
import jolie.lang.parse.ast.CurrentHandlerStatement;
import jolie.lang.parse.ast.DeepCopyStatement;
import jolie.lang.parse.ast.DefinitionCallStatement;
import jolie.lang.parse.ast.DefinitionNode;
import jolie.lang.parse.ast.DivideAssignStatement;
import jolie.lang.parse.ast.DocumentationComment;
import jolie.lang.parse.ast.EmbedServiceNode;
import jolie.lang.parse.ast.EmbeddedServiceNode;
import jolie.lang.parse.ast.ExecutionInfo;
import jolie.lang.parse.ast.ExitStatement;
import jolie.lang.parse.ast.ForEachArrayItemStatement;
import jolie.lang.parse.ast.ForEachSubNodeStatement;
import jolie.lang.parse.ast.ForStatement;
import jolie.lang.parse.ast.IfStatement;
import jolie.lang.parse.ast.ImportStatement;
import jolie.lang.parse.ast.InputPortInfo;
import jolie.lang.parse.ast.InstallFixedVariableExpressionNode;
import jolie.lang.parse.ast.InstallStatement;
import jolie.lang.parse.ast.InterfaceDefinition;
import jolie.lang.parse.ast.InterfaceExtenderDefinition;
import jolie.lang.parse.ast.LinkInStatement;
import jolie.lang.parse.ast.LinkOutStatement;
import jolie.lang.parse.ast.MultiplyAssignStatement;
import jolie.lang.parse.ast.NDChoiceStatement;
import jolie.lang.parse.ast.NotificationOperationStatement;
import jolie.lang.parse.ast.NullProcessStatement;
import jolie.lang.parse.ast.OLSyntaxNode;
import jolie.lang.parse.ast.OneWayOperationDeclaration;
import jolie.lang.parse.ast.OneWayOperationStatement;
import jolie.lang.parse.ast.OutputPortInfo;
import jolie.lang.parse.ast.ParallelStatement;
import jolie.lang.parse.ast.PointerStatement;
import jolie.lang.parse.ast.PostDecrementStatement;
import jolie.lang.parse.ast.PostIncrementStatement;
import jolie.lang.parse.ast.PreDecrementStatement;
import jolie.lang.parse.ast.PreIncrementStatement;
import jolie.lang.parse.ast.Program;
import jolie.lang.parse.ast.ProvideUntilStatement;
import jolie.lang.parse.ast.RequestResponseOperationDeclaration;
import jolie.lang.parse.ast.RequestResponseOperationStatement;
import jolie.lang.parse.ast.RunStatement;
import jolie.lang.parse.ast.Scope;
import jolie.lang.parse.ast.SequenceStatement;
import jolie.lang.parse.ast.ServiceNode;
import jolie.lang.parse.ast.SolicitResponseOperationStatement;
import jolie.lang.parse.ast.SpawnStatement;
import jolie.lang.parse.ast.SubtractAssignStatement;
import jolie.lang.parse.ast.SynchronizedStatement;
import jolie.lang.parse.ast.ThrowStatement;
import jolie.lang.parse.ast.TypeCastExpressionNode;
import jolie.lang.parse.ast.UndefStatement;
import jolie.lang.parse.ast.ValueVectorSizeExpressionNode;
import jolie.lang.parse.ast.VariablePathNode;
import jolie.lang.parse.ast.WhileStatement;
import jolie.lang.parse.ast.courier.CourierChoiceStatement;
import jolie.lang.parse.ast.courier.CourierDefinitionNode;
import jolie.lang.parse.ast.courier.NotificationForwardStatement;
import jolie.lang.parse.ast.courier.SolicitResponseForwardStatement;
import jolie.lang.parse.ast.expression.AndConditionNode;
import jolie.lang.parse.ast.expression.ConstantBoolExpression;
import jolie.lang.parse.ast.expression.ConstantDoubleExpression;
import jolie.lang.parse.ast.expression.ConstantIntegerExpression;
import jolie.lang.parse.ast.expression.ConstantLongExpression;
import jolie.lang.parse.ast.expression.ConstantStringExpression;
import jolie.lang.parse.ast.expression.FreshValueExpressionNode;
import jolie.lang.parse.ast.expression.InlineTreeExpressionNode;
import jolie.lang.parse.ast.expression.InstanceOfExpressionNode;
import jolie.lang.parse.ast.expression.IsTypeExpressionNode;
import jolie.lang.parse.ast.expression.NotExpressionNode;
import jolie.lang.parse.ast.expression.OrConditionNode;
import jolie.lang.parse.ast.expression.ProductExpressionNode;
import jolie.lang.parse.ast.expression.SumExpressionNode;
import jolie.lang.parse.ast.expression.VariableExpressionNode;
import jolie.lang.parse.ast.expression.VoidExpressionNode;
import jolie.lang.parse.ast.types.BasicTypeDefinition;
import jolie.lang.parse.ast.types.TypeChoiceDefinition;
import jolie.lang.parse.ast.types.TypeDefinitionLink;
import jolie.lang.parse.ast.types.TypeInlineDefinition;
import jolie.lang.parse.context.ParsingContext;
import jolie.util.Pair;
import staticTypechecker.entities.ChoiceType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.Type;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.utils.BasicTypeUtils;
import staticTypechecker.utils.ToString;
import staticTypechecker.utils.TypeUtils;
import staticTypechecker.utils.TypeConverter;
import staticTypechecker.entities.Module;
import staticTypechecker.entities.Operation;
import staticTypechecker.entities.OutputPort;
import staticTypechecker.entities.Path;
import staticTypechecker.entities.Fault;
import staticTypechecker.entities.Handler;
import staticTypechecker.entities.Tau;
import staticTypechecker.entities.Service;
import staticTypechecker.faults.FaultHandler;
import staticTypechecker.faults.FaultFault;
import staticTypechecker.faults.MiscFault;
import staticTypechecker.faults.TypeFault;
import staticTypechecker.faults.UnknownFunctionFault;
import staticTypechecker.faults.WarningHandler;

/**
 * Synthesizer for a parsed Jolie abstract syntax tree. Synthesizes the type of each node.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class Synthesizer implements OLVisitor<Tau, Tau> {
	private static HashMap<String, Synthesizer> synths = new HashMap<>(); 	// a static map, which maps module paths to synthesizers
	private Module module; 													// the module of this synthesizer
	private Stack<ArrayList<Path>> pathsAlteredInWhile = new Stack<>();		// the stack to keep track of the paths to the nodes changed during a while loop
	private Stack<String> scopes=new Stack<>();
	private Service service = null;											// the service object of the service the synthesizer currently is synthezing. This is needed to find operations
	
	/**
	 * Get a synthesizer for the given module
	 * @param module the module of the synthesizer
	 * @return the Synthesizer for the module
	 */
	public static Synthesizer get(Module module){
		Synthesizer ret = Synthesizer.synths.get(module.fullPath());

		if(ret == null){
			ret = new Synthesizer(module);
			Synthesizer.synths.put(module.fullPath(), ret);
		}
	
		return ret;
	}
	
	private Synthesizer(Module module){
		this.module = module;
	}

	/**
	 * Synthesizes the program of the module of this Synthesizer
	 * @return the final type tree for the module 
	 */
	public Tau synthesize(){
		Tau t=new Tau(Type.VOID());
		return this.module.program().accept(this, t);
	}

	/**
	 * Synthesizes the type of the given node
	 * @param node the node to synthesize the type of
	 * @param T the initial type tree. This tree will not be changed
	 * @return the type tree after the node has been synthesized
	 */
	public Tau synthesize(OLSyntaxNode node, Tau T){
		return node.accept(this, T);
	}

	public Tau visit(Program p, Tau T){
		for(OLSyntaxNode n : p.children()){
			T = this.synthesize(n, T);
		}
		return T;
	}

	public Tau visit(TypeInlineDefinition t, Tau T){
		return T;
	}

	public Tau visit( OneWayOperationDeclaration decl, Tau T ){
		return T;
	};

	public Tau visit( RequestResponseOperationDeclaration decl, Tau T ){
		return T;
	};

	public Tau visit( DefinitionNode n, Tau T ){
		return this.synthesize(n.body(), T);
	};

	public Tau visit( ParallelStatement n, Tau T ){
		return T;
	};

	public Tau visit( SequenceStatement n, Tau T ){
		for(OLSyntaxNode child : n.children()){
			Tau temp = this.synthesize(child, T);
			T=Tau.join(T,temp);
		}
		return T;
	};

	public Tau visit( NDChoiceStatement n, Tau T ){
		ArrayList<Type> trees = new ArrayList<>(n.children().size()); // save all the possible outcomes

		for(int i = 0; i < n.children().size(); i++){
			Tau T1 = this.synthesize(n.children().get(i).key(), T); // synthesize the label (in the [])
			Tau T2 = this.synthesize(n.children().get(i).value(), T1); // synthesize the behaviour (in the {})
			trees.add(T2.getType());
		}
		
		if(trees.size() == 1){
			T.changeType(trees.get(0));
			return T;
	
		}
		else{
			Type T3 = new ChoiceType(trees);	
			T.changeType(T3);
			return T;
		}
	};

	public Tau visit( OneWayOperationStatement n, Tau T ){
		Service service = this.service;
		Operation op = service.getOperation(n.id());
		if(op == null){
			FaultHandler.throwFault(new UnknownFunctionFault("The operation '" + n.id() + "' is unknown in service '" + service.name() + "'. Maybe you forgot to give the service an inputPort with an interface which provides the operation?", n.context()), false);
			return T;
		}

		Type T_in = op.requestType(); // the data type which is EXPECTED by the oneway operation
		Path p_in = new Path(n.inputVarPath().path()); // the path to the node which is given as input to the operation

		// update the node at the path to the input type
		Type T1 = T.getType().shallowCopyExcept(p_in);
		TypeUtils.setTypeOfNodeByPath(p_in, T_in, T1);

		for(ArrayList<Path> a : this.pathsAlteredInWhile){
			a.add(p_in);
		}
		T.changeType(T1);
		return T;
	};

	public Tau visit( RequestResponseOperationStatement n, Tau T ){
		Service service = this.service;
		Operation op = service.getOperation(n.id());
		if(op == null){
			FaultHandler.throwFault(new UnknownFunctionFault("The operation '" + n.id() + "' is unknown in service '" + service.name() + "'. Maybe you forgot to give the service an inputPort with an interface which provides the operation?", n.context()), false);
			return T;
		}
		
		Type T_in = op.requestType(); // the type of the data the operation EXPECTS as input
		Type T_out = op.responseType(); // the type of the data RETURNED from the operation
		
		Path p_in = new Path(n.inputVarPath().path()); // the path to the node which is given AS INPUT to the operation
		Path p_out = new Path(n.outputExpression().toString()); // the path to the node in which the OUTPUT of the operation is stored

		// given that p_in is of type T_in find the type of the behaviour
		Type T_update = T.getType().shallowCopyExcept(p_in);
		TypeUtils.setTypeOfNodeByPath(p_in, T_in, T_update);
		T.changeType(T_update);
		Tau T1 = this.synthesize(n.process(), T);
		
		// check that p_out is a subtype of T_out 
		ArrayList<Type> possibleTypes = TypeUtils.findNodesExact(p_out, T1.getType(), true, false); // the possible types of p_out after the behaviour
		
		Type p_out_type;
		if(possibleTypes.size() == 1){
			p_out_type = possibleTypes.get(0);
		}
		else{
			p_out_type = new ChoiceType(possibleTypes);
		}

		this.check(p_out_type, T_out, n.context(), "operation \"" + op.name() + "\" does not have the expected return type.\nActual return type:\n" + p_out_type.prettyString() + "\n\nExpected return type:\n" + T_out.prettyString());

		return T1;
	};

	public Tau visit( NotificationOperationStatement n, Tau T ){
		OutputPort port = (OutputPort)this.module.symbols().get(n.outputPortId(), SymbolType.OUTPUT_PORT);
		Operation op = port.getOperation(n.id());
		if(op == null){
			FaultHandler.throwFault(new UnknownFunctionFault("The operation '" + n.id() + "' is unknown in outputPort '" + port.name(), n.context()), false);
			return T;
		}

		String operationName = op.name();
		String outputPortName = n.outputPortId();
		
		// if the notify is an assertion, it is a typehint
		if(operationName.equals("assert") && outputPortName.equals(System.getProperty("typehint"))){
			if(!(n.outputExpression() instanceof InstanceOfExpressionNode)){
				FaultHandler.throwFault(new MiscFault("argument given to assert must be an instanceof expression", n.context()), true);
			}
			InstanceOfExpressionNode parsedNode = (InstanceOfExpressionNode)n.outputExpression();
			OLSyntaxNode expression = parsedNode.expression();

			if(!(expression instanceof VariableExpressionNode)){
				FaultHandler.throwFault(new MiscFault("first argument of instanceof must be a path to a variable", n.context()), true);
			}
			
			Type type = TypeConverter.convert(parsedNode.type(), this.module.symbols());
			Tau typeOfEx = this.synthesize(expression, T);
			String nameOfExpression = ToString.of(expression);
			this.check(typeOfEx.getType(), type, n.context(), "the type of '" + nameOfExpression + "' is not a subtype of the typehint. Type of " + nameOfExpression + ":\n" + typeOfEx.getType().prettyString() + "\n\nTypehint given:\n" + type.prettyString(), true);

			Path path = new Path(((VariableExpressionNode)expression).variablePath().path());
			Type T1 = T.getType().shallowCopyExcept(path);
			TypeUtils.setTypeOfNodeByPath(path, type, T1);
			T.changeType(T1);
			return T;
		}
		
		// else it is just a normal oneway invocation
		Tau T3=new Tau(Type.VOID());

		Type T_out = op.requestType(); // the type of the data which is EXPECTED of the oneway operation
		Tau p_out = n.outputExpression() != null ? this.synthesize(n.outputExpression(), T) : Tau.join(T, T3); // the type which is GIVEN to the oneway operation

		this.check(p_out.getType(), T_out, n.context(), "Type given to \"" + op.name() + "\" is different from what is expected. Given type:\n" + p_out.getType().prettyString() + "\n\nExpected type:\n" + T_out.prettyString());

		return T;
	};

	public Tau visit( SolicitResponseOperationStatement n, Tau T ){
		OutputPort port = (OutputPort)this.module.symbols().get(n.outputPortId(), SymbolType.OUTPUT_PORT);
		Operation op = port.getOperation(n.id());
		if(op == null){
			FaultHandler.throwFault(new UnknownFunctionFault("The operation '" + n.id() + "' is unknown in outputPort '" + port.name(), n.context()), false);
			return T;
		}

		Type T_in = op.responseType(); // the type of the data which is RETURNED by the reqres operation
		Type T_out = op.requestType(); // the type of the data which is EXPECTED of the reqres operation
		
		Path p_in; // the path to the node in which to store the returned data

		if(n.inputVarPath() == null){ // no input path given
			p_in = new Path();
		}
		else{
			p_in = new Path(n.inputVarPath().path());
		}
		Tau T3=new Tau(Type.VOID());
		Tau p_out = n.outputExpression() != null ? this.synthesize(n.outputExpression(), T) : Tau.join(T, T3); // the type which is GIVEN to the reqres operation
		
		// check that p_out is subtype of T_out
		this.check(p_out.getType(), T_out, n.context(), "Type given to \"" + op.name() + "\" is different from what is expected. Given type:\n" + p_out.getType().prettyString() + "\n\nExpected type:\n" + T_out.prettyString());

		// update type of p_in to T_in
		Type T1 = T.getType().shallowCopyExcept(p_in);
		TypeUtils.setTypeOfNodeByPath(p_in, T_in, T1);

		for(ArrayList<Path> a : this.pathsAlteredInWhile){
			a.add(p_in);
		}
		T.changeType(T1);
		return T;
	};

	public Tau visit( LinkInStatement n, Tau T ){
		return null;
	};

	public Tau visit( LinkOutStatement n, Tau T ){
		return null;
	};

	public Tau visit( AssignStatement n, Tau T ){
		// retrieve the type of the expression
		OLSyntaxNode e = n.expression();
		Tau T_e = this.synthesize(e, T);
		
		// update the type of the node
		Path path = new Path(n.variablePath().path());
		Type T1 = T.getType().shallowCopyExcept(path);
		ArrayList<BasicTypeDefinition> basicTypes = Type.getBasicTypesOfNode(T_e.getType());
		TypeUtils.setBasicTypeOfNodeByPath(path, basicTypes, T1);

		for(ArrayList<Path> a : this.pathsAlteredInWhile){
			a.add(path);
		}
		T.changeType(T1);
		return T;
	};

	@Override
	public Tau visit(AddAssignStatement n, Tau T) {
		Path path = new Path(n.variablePath().path());

		Type T1 = T.getType().shallowCopyExcept(path);
		
		Tau typeOfLeftSide = this.synthesize(n.variablePath(), T);
		Tau typeOfRightSide = this.synthesize(n.expression(), T);
		this.deriveTypeAndUpdateNode(path, T1, OperandType.ADD, typeOfLeftSide.getType(), typeOfRightSide.getType(), n.context());

		T.changeType(T1);
		return T;
	}

	@Override
	public Tau visit(SubtractAssignStatement n, Tau T) {
		Path path = new Path(n.variablePath().path());

		Type T1 = T.getType().shallowCopyExcept(path);

		Tau typeOfLeftSide = this.synthesize(n.variablePath(), T);
		Tau typeOfRightSide = this.synthesize(n.expression(), T);
		this.deriveTypeAndUpdateNode(path, T1, OperandType.SUBTRACT, typeOfLeftSide.getType(), typeOfRightSide.getType(), n.context());

		T.changeType(T1);
		return T;
	}

	@Override
	public Tau visit(MultiplyAssignStatement n, Tau T) {
		Path path = new Path(n.variablePath().path());

		Type T1 = T.getType().shallowCopyExcept(path);

		Tau typeOfLeftSide = this.synthesize(n.variablePath(), T);
		Tau typeOfRightSide = this.synthesize(n.expression(), T);
		this.deriveTypeAndUpdateNode(path, T1, OperandType.MULTIPLY, typeOfLeftSide.getType(), typeOfRightSide.getType(), n.context());
		
		T.changeType(T1);
		return T;
	}

	@Override
	public Tau visit(DivideAssignStatement n, Tau T) {
		Path path = new Path(n.variablePath().path());

		Type T1 = T.getType().shallowCopyExcept(path);

		Tau typeOfLeftSide = this.synthesize(n.variablePath(), T);
		Tau typeOfRightSide = this.synthesize(n.expression(), T);

		this.deriveTypeAndUpdateNode(path, T1, OperandType.DIVIDE, typeOfLeftSide.getType(), typeOfRightSide.getType(), n.context());
		
		T.changeType(T1);
		return T;
	}

	/**
	 * Derives the resulting type between the calculation of "leftSide opType rightSide" and updates the basictype of the node at the end of the given path to this result
	 * @param path the path to the node to update
	 * @param tree the type tree in which the node resides
	 * @param opType the type of the operand
	 * @param leftSide the left side of the calculation
	 * @param rightSide the right side of the calculation
	 * @param ctx the parsing context of the calculation
	 */
	private void deriveTypeAndUpdateNode(Path path, Type tree, OperandType opType, Type leftSide, Type rightSide, ParsingContext ctx){
		List<BasicTypeDefinition> newBasicTypes = BasicTypeUtils.deriveTypeOfOperation(opType, leftSide, rightSide, ctx);
		TypeUtils.setBasicTypeOfNodeByPath(path, newBasicTypes, tree);

		for(ArrayList<Path> a : this.pathsAlteredInWhile){
			a.add(path);
		}
	}

	/**
	 * If statement
	 */
	public Tau visit( IfStatement n, Tau T ){
		ChoiceType resultType = new ChoiceType();

		for(Pair<OLSyntaxNode, OLSyntaxNode> p : n.children()){
			OLSyntaxNode expression = p.key();
			OLSyntaxNode body = p.value();

			Tau typeOfEx = this.synthesize(expression, T);
			this.check(typeOfEx.getType(), Type.BOOL(), n.context(), "Guard of if-statement is not subtype of bool { ? }. Found type:\n" + typeOfEx.getType().prettyString()); // check that expression is of type bool
			Tau T1 = this.synthesize(body, T);
			resultType.addChoiceUnsafe(T1.getType());
		}

		OLSyntaxNode elseProcess = n.elseProcess();
		if(elseProcess != null){ // there is an else clause
			resultType.addChoiceUnsafe(this.synthesize(elseProcess, T).getType());
		}
		else{ // there is not else clause, here we add the initial state as choice as well, since we may not enter the if statement
			resultType.addChoiceUnsafe(T.getType());
		}
		
		if(resultType.choices().size() == 1){
			T.changeType(resultType.choices().get(0));
			return T;
		}
			
		T.changeType(resultType);
		return T;
	};

	public Tau visit( DefinitionCallStatement n, Tau T ){
		return T;
	};

	/**
	 * While statement
	 */
	public Tau visit( WhileStatement n, Tau T ){
		this.pathsAlteredInWhile.push(new ArrayList<>());

		OLSyntaxNode condition = n.condition();
		OLSyntaxNode body = n.body();

		Tau typeOfCondition = this.synthesize(condition, T);
		this.check(typeOfCondition.getType(), Type.BOOL(), n.context(), "Guard of while loop is not of type bool. Found type:\n" + typeOfCondition.getType().prettyString()); // check that the initial condition is of type bool

		Tau originalState = T; // saved here, since it is used in the fallback plan

		// the return type is a conjunction between the original state and the one found through the iterations OR the fallback
		ChoiceType result = new ChoiceType();
		result.addChoiceUnsafe(originalState.getType());

		ChoiceType mergedState = new ChoiceType();
		mergedState.addChoiceUnsafe(originalState.getType());

		for(int i = 0; i < 10; i++){
			Tau R = this.synthesize(body, T); // synthesize the type of the body after an iteration
			typeOfCondition = this.synthesize(condition, R);
			this.check(typeOfCondition.getType(), Type.BOOL(), n.context(), "Guard of while loop is not of type bool. Found type:\n" + typeOfCondition.getType().prettyString()); // check that the initial condition is of type bool
			
			if(R.getType().isSubtypeOf(mergedState)){ // the new state is a subtype of one of the previous states (we have a steady state)
				result.addChoiceUnsafe(mergedState);
				this.pathsAlteredInWhile.pop();
				R.changeType(result);
				return R;
			}

			mergedState.addChoiceUnsafe(R.getType());
			T = R;
		}

		// we did not find a steady state in the while loop. Here we do the fallback plan, which is to undefine all variables changed in the while loop
		result.addChoiceUnsafe( TypeUtils.undefine(originalState.getType(), this.pathsAlteredInWhile.peek()) );

		WarningHandler.throwWarning("could not determine the resulting type of the while loop, affected types may be incorrect from here", n.context());
		
		this.pathsAlteredInWhile.pop();
		originalState.changeType(result.convertIfPossible());
		return originalState;
	};

	public Tau visit( OrConditionNode n, Tau T ){
		T.changeType(Type.BOOL());
		return T;
	};

	public Tau visit( AndConditionNode n, Tau T ){
		T.changeType(Type.BOOL());
		return T;
	};

	public Tau visit( NotExpressionNode n, Tau T ){
		T.changeType(Type.BOOL());
		return T;
	};

	public Tau visit( CompareConditionNode n, Tau T ){;
		T.changeType(Type.BOOL());
		return T;
	};

	public Tau visit( ConstantIntegerExpression n, Tau T ){
		T.changeType(Type.INT());
		return T;
	};

	public Tau visit( ConstantDoubleExpression n, Tau T ){
		T.changeType(Type.DOUBLE());
		return T;
	};

	public Tau visit( ConstantBoolExpression n, Tau T ){
		T.changeType(Type.BOOL());
		return T;
	};

	public Tau visit( ConstantLongExpression n, Tau T ){
		T.changeType(Type.LONG());
		return T;
	};

	public Tau visit( ConstantStringExpression n, Tau T ){
		 T.changeType(Type.STRING());
		 return T;
	};

	public Tau visit( ProductExpressionNode n, Tau T ){
		return this.getTypeOfSumOrProduct(n, T);
	};

	public Tau visit( SumExpressionNode n, Tau T ){
		return this.getTypeOfSumOrProduct(n, T);
	};

	private Tau getTypeOfSumOrProduct(OLSyntaxNode n, Tau T){
		List<Pair<OperandType, OLSyntaxNode>> operands;

		if(n instanceof SumExpressionNode){
			operands = ((SumExpressionNode)n).operands();
		}
		else{
			operands = ((ProductExpressionNode)n).operands();
		}

		Type currType = Type.VOID(); // set initial type to void to make sure it will be overwritten by any other type

		for(Pair<OperandType, OLSyntaxNode> pair : operands){
			OperandType currOp = pair.key();
			Tau nextType = this.synthesize(pair.value(), T);

			List<BasicTypeDefinition> basicTypes = BasicTypeUtils.deriveTypeOfOperation(currOp, currType, nextType.getType(), n.context());

			if(basicTypes.size() == 1){
				currType = new InlineType(basicTypes.get(0), null, null, false);
			}
			else{
				currType = ChoiceType.fromBasicTypes(basicTypes);
			}
		}

		 T.changeType(currType);
		 return T;
	}

	/**
	 * A variable path, such as x.y.z. Here we find the exact node at the end of the path and return it.
	 */
	public Tau visit( VariableExpressionNode n, Tau T ){
		Path path = new Path(n.variablePath().path());
		ArrayList<Type> types = TypeUtils.findNodesExact(path, T.getType(), false, false);

		if(types.isEmpty()){ // return void type if no nodes was found
			T.changeType(Type.VOID());
			return T;
		}
		else if(types.size() == 1){ // if only one node was found, return it
			T.changeType(types.get(0));
			return T;
		}
		else{ // if more nodes were found, return the disjunction between them
			Type T1= new ChoiceType(types);
			T.changeType(T1);
			return T;
		}
	};

	public Tau visit( NullProcessStatement n, Tau T ){
		return T;
	};

	public Tau visit( Scope n, Tau T ){
		int hcutof=T.hcutof();
		int fcutof=T.fcutof();
		T.sethcutof(T.gethandlers().size());
		T.setfcutof(T.getunhandledFaults().size());
		scopes.push(n.id());
		Tau R=this.synthesize(n.body(),T);
		if(R.ishandled()){
			System.out.println("test");
			Tau T1=this.synthesize(R.Handler().body(),T);
			T.reorder(hcutof,fcutof);
			T=Tau.join(T,T1);
		}else{
			System.out.println("test6");
			T.reorder(hcutof,fcutof);
			R.reorder(hcutof,fcutof);
			T=Tau.join(T, R);
		}
		T.reorder(hcutof,fcutof);
		scopes.pop();
		if(scopes.empty()){
			declareunhandled(T);
		}
		return T;
	};

	public Tau visit( InstallStatement n, Tau T ){
		for (Pair<String,OLSyntaxNode> P : n.handlersFunction().pairs()) {
		String id=P.key();
		OLSyntaxNode fun=P.value();	
		Handler H=new Handler(id, fun,scopes.peek());
		T.addHandler(H);
		}
		return T;
	};

	public Tau visit( CompensateStatement n, Tau T ){
		return T;
	};

	public Tau visit( ThrowStatement n, Tau T ){
			System.out.println("test2");
			Fault F=new Fault(n.id(), scopes.peek(), n.context(), n.expression());
			T.addFault(F);
			return T;
	};

	public Tau visit( ExitStatement n, Tau T ){
		//
		return T;
	};

	public Tau visit( ExecutionInfo n, Tau T ){
		return T;
	};

	public Tau visit( CorrelationSetInfo n, Tau T ){
		return T;
	};

	public Tau visit( InputPortInfo n, Tau T ){
		return T;
	};

	public Tau visit( OutputPortInfo n, Tau T ){
		return T;
	};

	public Tau visit( PointerStatement n, Tau T ){
		return T;
	};

	/**
	 * Deep copy
	 */
	public Tau visit( DeepCopyStatement n, Tau T ){
		Path leftPath = new Path(n.leftPath().path());
		
		Type T1 = T.getType().shallowCopyExcept(leftPath); // the node at the end of leftPath will be changed, so the entire path must be deep copied
		T1 = TypeUtils.unfold(leftPath, T1);

		ArrayList<InlineType> trees = new ArrayList<>(); // the trees of the initial state	
		if(T1 instanceof InlineType){ // only one tree
			trees.add((InlineType)T1);
		}
		else{ // case of disjunction, add all the choices as initial trees
			ChoiceType parsed = (ChoiceType)T1;
			trees = parsed.choices();
		}

		for(InlineType tree : trees){
			T.changeType(tree);
			Tau typeOfExpression = this.synthesize(n.rightExpression(),T );
	
			// find the nodes to update and their parents
			ArrayList<Pair<InlineType, String>> leftSideNodes = TypeUtils.findParentAndName(leftPath, tree, true, false);
	
			// update the nodes with the deep copied versions
			for(Pair<InlineType, String> pair : leftSideNodes){
				InlineType parent = pair.key();
				String childName = pair.value();
				Type child = parent.getChild(childName);

				Type resultOfDeepCopy = Type.deepCopy(child, typeOfExpression.getType());
				parent.addChildUnsafe(childName, resultOfDeepCopy);
			}
		}

		for(ArrayList<Path> a : this.pathsAlteredInWhile){
			a.add(leftPath);
		}

		T.changeType(T1);
		return T;
	};

	public Tau visit( RunStatement n, Tau T ){
		return T;
	};

	/**
	 * Undef statement. Here we find the parent of the path and remove the child
	 */
	public Tau visit( UndefStatement n, Tau T ){
		Path path = new Path(n.variablePath().path());
		Type T1 = T.getType().shallowCopyExcept(path);

		ArrayList<Pair<InlineType, String>> nodesToRemove = TypeUtils.findParentAndName(path, T1, false, false);

		for(Pair<InlineType, String> pair : nodesToRemove){
			pair.key().removeChildUnsafe(pair.value());
		}

		for(ArrayList<Path> a : this.pathsAlteredInWhile){
			a.add(path);
		}

		T.changeType(T1);
		return T;
	};

	public Tau visit( ValueVectorSizeExpressionNode n, Tau T ){
		return T;
	};

	public Tau visit( PreIncrementStatement n, Tau T ){
		return T;
	};

	public Tau visit( PostIncrementStatement n, Tau T ){
		return T;
	};

	public Tau visit( PreDecrementStatement n, Tau T ){
		return T;
	};

	public Tau visit( PostDecrementStatement n, Tau T ){
		return T;
	};

	public Tau visit( ForStatement n, Tau T ){
		return T;
	};

	public Tau visit( ForEachSubNodeStatement n, Tau T ){
		return T;
	};

	public Tau visit( ForEachArrayItemStatement n, Tau T ){
		return T;
	};

	public Tau visit( SpawnStatement n, Tau T ){
		return T;
	};

	public Tau visit( IsTypeExpressionNode n, Tau T ){
		return T;
	};

	public Tau visit( InstanceOfExpressionNode n, Tau T ){
		T.changeType(Type.BOOL());
		return T;
	};

	public Tau visit( TypeCastExpressionNode n, Tau T ){ 
	 Type T1 =	new InlineType(BasicTypeDefinition.of(n.type()), null, null, false);
	 T.changeType(T1);
	 return T;
	};

	public Tau visit( SynchronizedStatement n, Tau T ){
		return T;
	};

	public Tau visit( CurrentHandlerStatement n, Tau T ){
		//
		return T;
	};

	public Tau visit( EmbeddedServiceNode n, Tau T ){
		return T;
	};

	public Tau visit( InstallFixedVariableExpressionNode n, Tau T ){
		return T;
	};

	public Tau visit( VariablePathNode n, Tau T ){
		Path path = new Path(n.path());
		ArrayList<Type> types = TypeUtils.findNodesExact(path, T.getType(), false, false);

		if(types.size() == 1){
			T.changeType(types.get(0));
			return T;
		}
		else{
			Type T1 = new ChoiceType(types);
			 T.changeType(T1);
			 return T;
		}
	};

	public Tau visit( TypeDefinitionLink n, Tau T ){
		return T;
	};

	public Tau visit( InterfaceDefinition n, Tau T ){
		return T;
	};

	public Tau visit( DocumentationComment n, Tau T ){
		return T;
	};

	public Tau visit( FreshValueExpressionNode n, Tau T ){
		return T;
	};

	public Tau visit( CourierDefinitionNode n, Tau T ){
		return T;
	};

	public Tau visit( CourierChoiceStatement n, Tau T ){
		return T;
	};

	public Tau visit( NotificationForwardStatement n, Tau T ){
		return T;
	};

	public Tau visit( SolicitResponseForwardStatement n, Tau T ){
		return T;
	};

	public Tau visit( InterfaceExtenderDefinition n, Tau T ){
		return T;
	};

	public Tau visit( InlineTreeExpressionNode n, Tau T ){
		return T;
	};

	public Tau visit( VoidExpressionNode n, Tau T ){
		return T;
	};

	public Tau visit( ProvideUntilStatement n, Tau T ){
		return T;
	};

	public Tau visit( TypeChoiceDefinition n, Tau T ){
		return T;
	};

	public Tau visit( ImportStatement n, Tau T ){
		return T;
	};

	public Tau visit( ServiceNode n, Tau T ){
		Tau result;
		Tau T1 = T;

		if(n.parameterConfiguration().isPresent()){
			Path path = new Path(n.parameterConfiguration().get().variablePath());
			Type typeOfParam = (Type)this.module.symbols().get(n.parameterConfiguration().get().type().name(), SymbolType.TYPE);
			Type T2 = T.getType().shallowCopyExcept(path);

			TypeUtils.setTypeOfNodeByPath(path, typeOfParam, T2);
			T1.changeType(T2);
		}

		// synthesize the program of the service node
		this.service = (Service)this.module.symbols().get(n.name(), SymbolType.SERVICE);
		result = this.synthesize(n.program(), T1);
		this.service = null;

		return result;
	};

	public Tau visit( EmbedServiceNode n, Tau T ){
		return T;
	};

	/**
	 * Checks that the given T is a subtype of the given S. Throws a fault to FaultHandler if not.
	 * @param T the possible subtype
	 * @param S the possible supertype
	 * @param ctx the parsingcontext of the node where the subtype check happens
	 */
	public void check(Type T, Type S, ParsingContext ctx, String faultMessage){
		if(!T.isSubtypeOf(S)){
			FaultHandler.throwFault(new TypeFault(faultMessage, ctx), false);
		}
	}

	/**
	 * Checks that the given T is a subtype of the given S. Throws a fault to FaultHandler if not.
	 * @param T the possible subtype
	 * @param S the possible supertype
	 * @param ctx the parsingcontext of the node where the subtype check happens
	 * @param faultMessage the message to give the programmer in case of a fault
	 * @param terminate if true the execution of the type checker will stop, otherwise nothing happens
	 */
	public void check(Type T, Type S, ParsingContext ctx, String faultMessage, boolean terminate){
		if(!T.isSubtypeOf(S)){
			FaultHandler.throwFault(new TypeFault(faultMessage, ctx), terminate);
		}
	}
	public void declareunhandled(Tau T){
		ArrayList<Fault> Faults=T.getunhandledFaults();
		for (Fault fault : Faults) {
			String faultmessage="the throw fault"+fault.id()+"is not handled";
			FaultHandler.throwFault(new FaultFault(faultmessage,fault.parsingContext()),false);
		}		
	}
}
