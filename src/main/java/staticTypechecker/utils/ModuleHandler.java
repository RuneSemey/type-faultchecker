package staticTypechecker.utils;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import jolie.lang.parse.ast.ImportStatement;
import jolie.lang.parse.ast.OLSyntaxNode;
import staticTypechecker.visitors.TypeCheckerVisitor;
import staticTypechecker.entities.Module;

/**
 * Handles the Module instances of all the necessary Jolie modules.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class ModuleHandler {
	private static HashMap<String, Module> modules = new HashMap<>(); // maps module names to their Module instances

	/**
	 * Loads the Jolie module with the given name by parsing it and creating Module instances, which are saved in ModuleHandler. Also loads all dependency-modules (modules which are imported from this one). 
	 * @return a List of the loaded modules.
	 * @param moduleName the name of the module to load.
	 */
	public static List<Module> loadModule(String fullPath){
		ArrayList<Module> loadedModules = new ArrayList<>();

		String moduleName = ModuleHandler.getModuleName(fullPath);
		String pathToFolder = ModuleHandler.getPathToFolder(fullPath);

		Module module = new Module(moduleName, pathToFolder);

		ModuleHandler.modules.put(fullPath, module);

		loadedModules.add(module);

		for(OLSyntaxNode node : module.program().children()){
			if(node instanceof ImportStatement){
				ImportStatement n = (ImportStatement)node;
				String fullPathToImportedModule = ModuleHandler.findFullPath(n, module);

				if(ModuleHandler.contains(fullPathToImportedModule)){
					loadedModules.add(ModuleHandler.get(fullPathToImportedModule));
				}
				else{
					loadedModules.addAll( ModuleHandler.loadModule(fullPathToImportedModule) );
				}
			}
		}

		return loadedModules;
	}

	private static String getPathToFolder(String fullPath){
		String[] split = fullPath.split("/");
		
		if(split.length == 1){ // working with path of just the filename
			return ".";
		}
		
		// working with path like "a/b/c"
		int nameLength = split[split.length - 1].length();
		String path = fullPath.substring(0, fullPath.length() - nameLength - 1);
		return path;
	}

	private static String getModuleName(String fullPath){
		String[] split = fullPath.split("/");
		return split[split.length - 1];
	}

	/**
	 * Returns the full path to the module being imported in the given import statement.
	 * @param n the import statement.
	 * @return the full path.
	 */
	public static String findFullPath(ImportStatement n, Module importer){
		List<String> importPath = n.importTarget();

		if(importPath.get(0).equals("")){ // a relative path
			String relativePath = importPath.subList(1, importPath.size()).stream().collect(Collectors.joining("/"));
			return importer.path() + "/" + relativePath + ".ol";
		}

		String strPath = importPath.stream().collect(Collectors.joining("/")) + ".ol";
		Path path = Paths.get(strPath);

		if(Files.exists(path)){ // an actual file
			return strPath;
		}
		else{ // from the std lib
			return System.getenv("JOLIE_HOME") + "/packages/" + strPath;
		}
		
	}

	/**
	 * Runs the given visitor on all loaded modules twice. Firstly, it runs the visitor with the "processImports" set to false, secondly with it set to true.
	 * @param visitor the visitor to run.
	 */
	public static void runVisitor(TypeCheckerVisitor visitor){
		for(Module m : ModuleHandler.modules.values()){
			visitor.process(m, false);
		}
		for(Module m : ModuleHandler.modules.values()){
			visitor.process(m, true);
		}
		
	}

	public static Module get(String name){
		return ModuleHandler.modules.get(name);
	}

	public static boolean contains(String name){
		return ModuleHandler.modules.containsKey(name);
	}

	public static HashMap<String, Module> modules(){
		return ModuleHandler.modules;
	}
}
