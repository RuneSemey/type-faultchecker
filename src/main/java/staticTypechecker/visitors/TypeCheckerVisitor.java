package staticTypechecker.visitors;

import staticTypechecker.entities.Type;
import staticTypechecker.entities.Module;

/**
 * Simple interface which is implemented by the visitors in this folder
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public interface TypeCheckerVisitor {
	public Type process(Module m, boolean processImports);
}
