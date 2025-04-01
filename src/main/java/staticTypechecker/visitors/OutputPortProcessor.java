package staticTypechecker.visitors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

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
import jolie.lang.parse.ast.types.TypeChoiceDefinition;
import jolie.lang.parse.ast.types.TypeDefinitionLink;
import jolie.lang.parse.ast.types.TypeInlineDefinition;
import staticTypechecker.entities.SymbolTable;
import staticTypechecker.entities.Symbol.SymbolType;
import staticTypechecker.faults.FaultHandler;
import staticTypechecker.faults.NoServiceParameterFault;
import staticTypechecker.faults.PortsIncompatibleFault;
import staticTypechecker.entities.Type;
import staticTypechecker.entities.InputPort;
import staticTypechecker.entities.Interface;
import staticTypechecker.entities.Module;
import staticTypechecker.entities.Operation;
import staticTypechecker.entities.OutputPort;
import staticTypechecker.entities.Service;

/**
 * Goes through an AST of a given Jolie module and converts the outputPorts and embeddings. 
 * Also checks for compatibility between the interfaces in embed-in cases.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class OutputPortProcessor implements OLVisitor<SymbolTable, Void>, TypeCheckerVisitor {
	private Module module;
	public OutputPortProcessor(){}

	public Type process(Module module, boolean processImports){
		this.module = module;
		Program p = module.program();
		
		if(!processImports){
			for(OLSyntaxNode child : p.children()){
				child.accept(this, module.symbols());
			}
		}

		return null;
	}

	@Override
	public Void visit(Program p, SymbolTable symbols) {
		for(OLSyntaxNode child : p.children()){
			child.accept(this, symbols);
		}

		return null;
	}
 
	@Override
	public Void visit(ServiceNode n, SymbolTable symbols) {
		String serviceName = n.name();
		Service service = (Service)symbols.get(SymbolTable.newPair(serviceName, SymbolType.SERVICE));

		// for each output port of the service, create an OutputPort instance and add it to the symbol table and service object
		for(OLSyntaxNode child : n.program().children()){
			if(child instanceof OutputPortInfo){
				child.accept(this, symbols);

				String portName = ((OutputPortInfo)child).id();
				if(symbols.get(SymbolTable.newPair(portName, SymbolType.OUTPUT_PORT)) != null){ // we may not have created the output port here, but then it will be created in the for loop below with embeddings
					service.addOutputPort(portName, (OutputPort)symbols.get(portName, SymbolType.OUTPUT_PORT));
				}
			}
		}
		
		// for each embedding, create or use existing outputport and add it to the symbol table and service object
		for(OLSyntaxNode child : n.program().children()){
			if(child instanceof EmbedServiceNode){
				if(((EmbedServiceNode)child).bindingPort() != null){ // check if the embedded service is bound to a port or not, if not idk what to do
					child.accept(this, symbols);
	
					String portName = ((EmbedServiceNode)child).bindingPort().id();
					service.addOutputPort(portName, (OutputPort)symbols.get(portName, SymbolType.OUTPUT_PORT));
				}
			}
		}

		return null;
	}

	@Override
	public Void visit(OutputPortInfo n, SymbolTable symbols) {
		// ready the data
		String portName = n.id();
		String location = n.location() != null ? ((ConstantStringExpression)n.location()).value() : null; // the location of the port, if it exists, otherwise null
		String protocol = n.protocolId().equals("") ? null : n.protocolId(); // the id of the protocol, if it exsist, otherwise null

		HashSet<Interface> interfaces = new HashSet<>();
		for(InterfaceDefinition id : n.getInterfaceList()){
			String nameOfInterface = id.name();
			interfaces.add((Interface)symbols.get(nameOfInterface, SymbolType.INTERFACE));
		}

		OutputPort port = new OutputPort(portName, location, protocol, interfaces);

		symbols.put(SymbolTable.newPair(portName, SymbolType.OUTPUT_PORT), port);

		return null;
	}

	@Override
	public Void visit(EmbedServiceNode n, SymbolTable symbols) {
		String portName = n.bindingPort().id();
		String serviceName = n.serviceName();
		boolean isNewPort = n.isNewPort();
		Service service = (Service)symbols.get(SymbolTable.newPair(serviceName, SymbolType.SERVICE));

		if(service.parameter() != null){ // the service requires a parameter, check that the provided is a subtype
			OLSyntaxNode passingParameter = n.passingParameter();
			Type expectedType = service.parameter();
			
			// check that a parameter was actually provided
			if(passingParameter == null){
				FaultHandler.throwFault(new NoServiceParameterFault(service, n.context()), false);
				return null;
			}

			// check that the type of the parameter is a subtype of the expected type
			Type providedType = (Type)symbols.get(passingParameter.toString(), SymbolType.TYPE);
			if(providedType == null){ // the symbol is not in the symbol table, try to synthesize it
				providedType = Synthesizer.get(this.module).synthesize(passingParameter, null).getType();
			}

			Synthesizer.get(this.module).check(providedType, expectedType, n.context(), "Type given to service: \"" + serviceName + "\" is not of expected type. Type given:\n" + providedType.prettyString() + "\n\nType expected:\n" + expectedType.prettyString()); // check that it is a subtype
		}

		if(!isNewPort){ // this is an "embed-in" case, where we use an existing output port, check if interfaces are compatible
			HashMap<String, Operation> requiredOperations = new HashMap<>(); // maps operation names to operation objects of all operations the port requires
			HashMap<String, Operation> providedOperations = new HashMap<>(); // maps operation names to operation objects of all operations the service provides

			OutputPort bindingPort = (OutputPort)symbols.get(portName, SymbolType.OUTPUT_PORT);

			// find required operations
			for(Interface inter : bindingPort.interfaces()){ // loop through each implemented interface of the output port
				for(Entry<String, Operation> ent : inter.operations()){ // loop through each operation required by the interface
					requiredOperations.put(ent.getKey(), ent.getValue()); // add the operation as required
				}
			}

			// find provided operations
			for(Entry<String, InputPort> IPEnt : service.inputPorts()){ // loop through each input port of the service
				InputPort port = IPEnt.getValue();
				for(Interface inter : port.interfaces()){ // loop through each implemented interface of the port 
					for(Entry<String, Operation> OPEnt : inter.operations()){ // loop through each operation required by the interface
						providedOperations.put(OPEnt.getKey(), OPEnt.getValue()); // add the operation as provided
					}
				}
			}

			// check if all required operations are provided
			boolean portSatisfied = true;
			List<Operation> scapeGoatOps = new ArrayList<>();
			for(Entry<String, Operation> ent : requiredOperations.entrySet()){
				String operationName = ent.getKey();
				Operation op = ent.getValue();

				if(
					!providedOperations.containsKey(operationName) || 	// if the provided operations does not contain the required operation name
					!op.isCompatibleWith(providedOperations.get(operationName)) 	// if the two operations are not compatible
				){ 
					portSatisfied = false;
					scapeGoatOps.add(op);
				}
			}

			if(!portSatisfied){ // interface requirements are not met by the service
				FaultHandler.throwFault(new PortsIncompatibleFault(bindingPort, service, scapeGoatOps, n.context()), false);
			}
		}
		else{ // in the case of an "embed-as" output port, create a new and add it to symbols
			List<InputPort> inputPortsOfService = service.inputPorts().stream().map(ent -> ent.getValue()).filter(m -> m.location().equals("local")).collect(Collectors.toList()); // retrieve all inputports with the location "local"
			
			String location = "local";
			String protocol = "";
			HashSet<Interface> interfaces = new HashSet<>();

			// add all interfaces of all the input ports
			for(InputPort i : inputPortsOfService){
				interfaces.addAll(i.interfaces());
			}

			symbols.put(SymbolTable.newPair(portName, SymbolType.OUTPUT_PORT), new OutputPort(portName, location, protocol, interfaces));
		}

		return null;
	}

	@Override
	public Void visit(InputPortInfo n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(InterfaceDefinition n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(TypeInlineDefinition n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(TypeDefinitionLink n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(TypeChoiceDefinition n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ImportStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(DefinitionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(SequenceStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(OneWayOperationDeclaration decl, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(RequestResponseOperationDeclaration decl, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ParallelStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(NDChoiceStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(OneWayOperationStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(RequestResponseOperationStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(NotificationOperationStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(SolicitResponseOperationStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(LinkInStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(LinkOutStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(AssignStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(AddAssignStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(SubtractAssignStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(MultiplyAssignStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(DivideAssignStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(IfStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(DefinitionCallStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(WhileStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(OrConditionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(AndConditionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(NotExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(CompareConditionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ConstantIntegerExpression n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ConstantDoubleExpression n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ConstantBoolExpression n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ConstantLongExpression n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ConstantStringExpression n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ProductExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(SumExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(VariableExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(NullProcessStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(Scope n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(InstallStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(CompensateStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ThrowStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ExitStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ExecutionInfo n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(CorrelationSetInfo n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(PointerStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(DeepCopyStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(RunStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(UndefStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ValueVectorSizeExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(PreIncrementStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(PostIncrementStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(PreDecrementStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(PostDecrementStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ForStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ForEachSubNodeStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ForEachArrayItemStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(SpawnStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(IsTypeExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(InstanceOfExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(TypeCastExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(SynchronizedStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(CurrentHandlerStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(EmbeddedServiceNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(InstallFixedVariableExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(VariablePathNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(DocumentationComment n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(FreshValueExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(CourierDefinitionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(CourierChoiceStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(NotificationForwardStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(SolicitResponseForwardStatement n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(InterfaceExtenderDefinition n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(InlineTreeExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(VoidExpressionNode n, SymbolTable symbols) {
		return null;
	}

	@Override
	public Void visit(ProvideUntilStatement n, SymbolTable symbols) {
		return null;
	}
}
