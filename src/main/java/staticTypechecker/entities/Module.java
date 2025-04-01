package staticTypechecker.entities;

import java.io.IOException;

import jolie.Interpreter;
import jolie.cli.CommandLineException;
import jolie.cli.CommandLineParser;
import jolie.lang.CodeCheckingException;
import jolie.lang.parse.ParserException;
import jolie.lang.parse.SemanticVerifier;
import jolie.lang.parse.ast.Program;
import jolie.lang.parse.module.ModuleException;
import jolie.lang.parse.util.ParsingUtils;

/**
 * Represents a Jolie module (file).
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class Module {
	private String name;			// name of the module
	private String path;			// the path to the folder of the module
	private Program program;		// the Program parsed by the Jolie parser
	private SymbolTable symbols;	// the symbol table of the module

	/**
	 * Creates a new Module.
	 * @param name the name of the MODULE, e.g. "myModule.ol".
	 * @param path the path to the FOLDER containing the module, e.g. "/some/path"
	 */
	public Module(String name, String path){
		this.name = name;
		this.path = path;
		this.program = Module.parseProgram(path + "/" + name);
		this.symbols = null;
	}

	/**
	 * @return the name of this Module, e.g. "myModule.ol".
	 */
	public String name(){
		return this.name;
	}

	/**
	 * @return the path to the folder containing this Module, e.g. "/some/path".
	 */
	public String path(){
		return this.path;
	}

	/**
	 * @return the full path to this Module, e.g. "/some/path/myModule.ol"
	 */
	public String fullPath(){
		return this.path + "/" + this.name;
	}

	/**
	 * @return the Program of this Module.
	 */
	public Program program(){
		return this.program;
	}

	/**
	 * @return the SymbolTable of this Module.
	 */
	public SymbolTable symbols(){
		return this.symbols;
	}

	/**
	 * Set the SymbolTable of this Module.
	 * @param newSymbols the new SymbolTable.
	 */
	public void setSymbols(SymbolTable newSymbols){
		this.symbols = newSymbols;
	}

	/**
	 * Parses the Jolie module at the given path.
	 * @param path the full path to the module.
	 * @return the Program of the module if the parsing was successfull.
	 */
	private static Program parseProgram(String path){
		String[] args = {path};

		try{
			final CommandLineParser cmdParser = new CommandLineParser(args, Module.class.getClassLoader());

			Interpreter.Configuration intConf = cmdParser.getInterpreterConfiguration();

			SemanticVerifier.Configuration semVerConfig = new SemanticVerifier.Configuration( intConf.executionTarget() );
			semVerConfig.setCheckForMain( false );
			
			String[] hardcodedPathToStdLib = {System.getenv("JOLIE_HOME") + "/packages"};

			Program program = ParsingUtils.parseProgram(
				intConf.inputStream(),
				intConf.programFilepath().toURI(),
				intConf.charset(),
				intConf.includePaths(),
				hardcodedPathToStdLib,
				intConf.jolieClassLoader(),
				intConf.constants(),
				semVerConfig,
				true 
			);

			cmdParser.close();

			return program;
		}
		catch(CommandLineException | IOException | ParserException | CodeCheckingException | ModuleException e){
			System.out.println("Error parsing module: " + path + ":\n" + e);
			System.exit(0);
			return null;
		}
	}
}
