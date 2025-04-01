package staticTypechecker.utils;

import java.util.List;
import java.util.stream.Collectors;

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
import jolie.lang.parse.ast.types.TypeChoiceDefinition;
import jolie.lang.parse.ast.types.TypeDefinitionLink;
import jolie.lang.parse.ast.types.TypeInlineDefinition;
import jolie.util.Pair;
import staticTypechecker.entities.Path;

/**
 * Visitor for pretty printing entities in Jolie.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class ToString implements OLVisitor<Void, String> {
	private static ToString stringifier = new ToString();
	private ToString(){};
	
	public static String of(OLSyntaxNode node){
		return node.accept(stringifier, null);
	}

	public String visit(Program p, Void v){
		return p.getClass().toString();
	}

	public String visit(TypeInlineDefinition t, Void v){
		return t.getClass().toString();
	}

	public String visit( OneWayOperationDeclaration decl, Void v ){
		return decl.getClass().toString();
	};

	public String visit( RequestResponseOperationDeclaration decl, Void v ){
		return decl.getClass().toString();
	};

	public String visit( DefinitionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ParallelStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( SequenceStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( NDChoiceStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( OneWayOperationStatement n, Void v ){
		Path inputPath = new Path(n.inputVarPath().path()); 
		String name = n.id();
		return name + "(" + inputPath + ")";
	};

	public String visit( RequestResponseOperationStatement n, Void v ){
		Path inputPath = new Path(n.inputVarPath().path()); 
		Path outputPath = new Path(n.outputExpression().toString());
		String name = n.id();
		return name + "(" + inputPath + ")(" + outputPath + ")";
	};

	public String visit( NotificationOperationStatement n, Void v ){
		String name = n.id();
		String portId = n.outputPortId();
		String outputPath = n.outputExpression().accept(this, null);
		return name + "@" + portId + "(" + outputPath + ")";
	};

	public String visit( SolicitResponseOperationStatement n, Void v ){
		String name = n.id();
		Path inputPath = n.inputVarPath() != null ? new Path(n.inputVarPath().path()) : new Path("");
		String portId = n.outputPortId();
		String outputPath = n.outputExpression() != null ? n.outputExpression().accept(this, null) : "";

		return name + "@" + portId + "(" + outputPath + ")(" + inputPath + ")";
	};

	public String visit( LinkInStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( LinkOutStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( AssignStatement n, Void v ){
		Path path = new Path(n.variablePath().path());
		String expression = n.expression().accept(this, null);
		return path + " = " + expression;
	};

	public String visit( AddAssignStatement n, Void v ){
		Path path = new Path(n.variablePath().path());
		String expression = n.expression().accept(this, null);

		return path + " += " + expression;
	};

	public String visit( SubtractAssignStatement n, Void v ){
		Path path = new Path(n.variablePath().path());
		String expression = n.expression().accept(this, null);

		return path + " -= " + expression;
	};

	public String visit( MultiplyAssignStatement n, Void v ){
		Path path = new Path(n.variablePath().path());
		String expression = n.expression().accept(this, null);

		return path + " *= " + expression;
	};

	public String visit( DivideAssignStatement n, Void v ){
		Path path = new Path(n.variablePath().path());
		String expression = n.expression().accept(this, null);

		return path + " /= " + expression;
	};

	public String visit( IfStatement n, Void v ){
		String result = "";

		for(Pair<OLSyntaxNode, OLSyntaxNode> p : n.children()){
			result += "if(" + p.key().accept(this, null) + ")\n";
		}

		return result;
	};

	public String visit( DefinitionCallStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( WhileStatement n, Void v ){
		String condition = n.condition().accept(this, null);
		return "while(" + condition + ")";
	};

	public String visit( OrConditionNode n, Void v ){
		return "(" + n.children().stream().map(c -> c.accept(this, null)).collect(Collectors.joining(" || ")) + ")";
	};

	public String visit( AndConditionNode n, Void v ){
		return "(" + n.children().stream().map(c -> c.accept(this, null)).collect(Collectors.joining(" && ")) + ")";
	};

	public String visit( NotExpressionNode n, Void v ){
		return "!(" + n.expression().accept(this, null) + ")";
	};

	public String visit( CompareConditionNode n, Void v ){
		String leftEx = n.leftExpression().accept(this, null);
		String rightEx = n.rightExpression().accept(this, null);
		String operand = n.opType().toString();

		if(operand.equals("RANGLE")){
			operand = ">";
		}
		else if(operand.equals("LANGLE")){
			operand = "<";
		}
		else if(operand.equals("MINOR_OR_EQUAL")){
			operand = "<=";
		}
		else if(operand.equals("MAJOR_OR_EQUAL")){
			operand = ">=";
		}
		else if(operand.equals("EQUAL")){
			operand = "==";
		}
		else if(operand.equals("NOT_EQUAL")){
			operand = "==";
		}

		return leftEx + " " + operand + " " + rightEx;
	};

	public String visit( ConstantIntegerExpression n, Void v ){
		return String.valueOf(n.value());
	};

	public String visit( ConstantDoubleExpression n, Void v ){
		return String.valueOf(n.value());
	};

	public String visit( ConstantBoolExpression n, Void v ){
		return String.valueOf(n.value());
	};

	public String visit( ConstantLongExpression n, Void v ){
		return String.valueOf(n.value()) + "L";
	};

	public String visit( ConstantStringExpression n, Void v ){
		return "\"" + n.value() + "\"";
	};

	private String sumToString(List<Pair<OperandType, OLSyntaxNode>> operands){
		String result = operands.get(0).value().accept(this, null); // string of first term

		for(int i = 1; i < operands.size(); i++){
			OperandType op = operands.get(i).key();
			OLSyntaxNode term = operands.get(i).value();

			switch(op){
				case ADD:
					result += " + ";
					break;
				case SUBTRACT:
					result += " - ";
					break;
				case MULTIPLY:
					result += " * ";
					break;
				case DIVIDE:
					result += " / ";
					break;
				case MODULUS:
					result += " % ";
					break;
			}

			result += term.accept(this, null);
		}

		return result;
	}

	public String visit( ProductExpressionNode n, Void v ){
		return sumToString(n.operands());
	};

	public String visit( SumExpressionNode n, Void v ){
		return sumToString(n.operands());
	};

	public String visit( VariableExpressionNode n, Void v ){
		Path path = new Path(n.variablePath().path());
		return path.toString();
	};

	public String visit( NullProcessStatement n, Void v ){
		return "null-process";
	};

	public String visit( Scope n, Void v ){
		return n.getClass().toString();
	};

	public String visit( InstallStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( CompensateStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ThrowStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ExitStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ExecutionInfo n, Void v ){
		return n.getClass().toString();
	};

	public String visit( CorrelationSetInfo n, Void v ){
		return n.getClass().toString();
	};

	public String visit( InputPortInfo n, Void v ){
		return n.getClass().toString();
	};

	public String visit( OutputPortInfo n, Void v ){
		return n.getClass().toString();
	};

	public String visit( PointerStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( DeepCopyStatement n, Void v ){
		Path path = new Path(n.leftPath().path());
		String expression = n.rightExpression().accept(this, null);

		return path + " << " + expression;
	};

	public String visit( RunStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( UndefStatement n, Void v ){
		Path path = new Path(n.variablePath().path());
		return "undef(" + path + ")";
	};

	public String visit( ValueVectorSizeExpressionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( PreIncrementStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( PostIncrementStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( PreDecrementStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( PostDecrementStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ForStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ForEachSubNodeStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ForEachArrayItemStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( SpawnStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( IsTypeExpressionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( InstanceOfExpressionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( TypeCastExpressionNode n, Void v ){
		return n.type().toString().toLowerCase() + "(" + n.expression().accept(this, null) + ")";
	};

	public String visit( SynchronizedStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( CurrentHandlerStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( EmbeddedServiceNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( InstallFixedVariableExpressionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( VariablePathNode n, Void v ){
		Path path = new Path(n.path());
		return path.toString();
	};

	public String visit( TypeDefinitionLink n, Void v ){
		return n.getClass().toString();
	};

	public String visit( InterfaceDefinition n, Void v ){
		return n.getClass().toString();
	};

	public String visit( DocumentationComment n, Void v ){
		return n.getClass().toString();
	};

	public String visit( FreshValueExpressionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( CourierDefinitionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( CourierChoiceStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( NotificationForwardStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( SolicitResponseForwardStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( InterfaceExtenderDefinition n, Void v ){
		return n.getClass().toString();
	};

	public String visit( InlineTreeExpressionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( VoidExpressionNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ProvideUntilStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( TypeChoiceDefinition n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ImportStatement n, Void v ){
		return n.getClass().toString();
	};

	public String visit( ServiceNode n, Void v ){
		return n.getClass().toString();
	};

	public String visit( EmbedServiceNode n, Void v ){
		return n.getClass().toString();
	};
}

