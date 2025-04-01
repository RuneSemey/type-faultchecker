package staticTypechecker.entities;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import jolie.lang.NativeType;
import jolie.lang.parse.ast.types.BasicTypeDefinition;
import jolie.util.Range;
import staticTypechecker.entities.Symbol.SymbolType;

/**
 * Represents the symbol table for a module.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class SymbolTable {
	private HashMap<Pair<String, SymbolType>, Symbol> table; // maps the name of the symbol to a pair containing the type of the symbol and the symbol itself
	
	public SymbolTable(){
		this.table = new HashMap<>();
		this.addBasicTypes();
	}

	/**
	 * Adds the basic types of Jolie to this symbol table, i.e. int, string, boolean etc.
	 */
	private void addBasicTypes(){
		NativeType[] baseTypes = NativeType.values();
		for(NativeType t : baseTypes){
			InlineType typeStruct = new InlineType(BasicTypeDefinition.of(t), new Range(1, 1), null, false);

			this.table.put(SymbolTable.newPair(t.id(), SymbolType.TYPE), typeStruct);
		}

		this.table.put(SymbolTable.newPair("undefined", SymbolType.TYPE), new InlineType(BasicTypeDefinition.of(NativeType.ANY), new Range(1, 1), null, true));
	}

	/**
	 * @param name the name to look for.
	 * @return true if the given name is present in this SymbolTable, regardless of type, false otherwise.
	 */
	public boolean containsKey(String name){
		return this.table.containsKey(new Pair<>(name, null));
	}

	/**
	 * @param key the key to look for.
	 * @return true if the given key is present in this SymbolTable, false otherwise.
	 */
	public boolean containsKey(Pair<String, SymbolType> key){
		return this.table.containsKey(key);
	}

	/**
	 * @param name the name of the Symbol to look for.
	 * @param type the type of the Symbol to look for.
	 * @return the Symbol with the key and type given if it is present in this SymbolTable, null otherwise.
	 */
	public Symbol get(String name, SymbolType type){
		Pair<String, SymbolType> p = new Pair<>(name, type);

		if(!this.containsKey(p)){
			return null;
		}

		return this.table.get(p);
	}

	/**
	 * @param key the name and the type of the Symbol to look for.
	 * @return the Symbol with the key given if it is present in this SymbolTable, null otherwise.
	 */
	public Symbol get(Pair<String, SymbolType> key){
		if(!this.containsKey(key)){
			return null;
		}

		return this.table.get(key);
	}

	/**
	 * @param name the name of the Symbols to look for.
	 * @return a List of all Symbols with the given name present in this SymbolTable.
	 */
	public List<Pair<Symbol, SymbolType>> getAllSymbols(String name){
		ArrayList<Pair<Symbol, SymbolType>> ret = new ArrayList<>();

		for(Entry<Pair<String, SymbolType>, Symbol> ent : this.entrySet()){
			String symbolName = ent.getKey().key();
			
			if(symbolName.equals(name)){
				SymbolType type = ent.getKey().value();
				Symbol symbol = ent.getValue();
				ret.add(SymbolTable.newPair(symbol, type));
			}
		}

		return ret;
	}

	/**
	 * Adds a new entry to this SymbolTable with the given key and object.
	 * @param key the key to add.
	 * @param symbol the Symbol to add.
	 */
	public void put(Pair<String, SymbolType> key, Symbol symbol){
		this.table.put(key, symbol);
	}

	/**
	 * @return the entryset of Symbols in this SymbolTable.
	 */
	public Collection<Entry<Pair<String, SymbolType>, Symbol>> entrySet(){
		return this.table.entrySet();
	}

	public String toString(){
		String res = "";
		for(Entry<Pair<String, SymbolType>, Symbol> ent : this.table.entrySet()){
			res += ent.getKey() + " = " + ent.getValue() + "\n";
		}

		return res;
	}

	public static Pair<String, SymbolType> newPair(String key, SymbolType value){
		return new Pair<String, SymbolType>(key, value);
	}

	public static Pair<Symbol, SymbolType> newPair(Symbol key, SymbolType value){
		return new Pair<Symbol, SymbolType>(key, value);
	}

	public static class Pair<K, V> {
		private K key;
		private V value;
		
		public Pair(K key, V value){
			this.key = key;
			this.value = value;
		}
	
		public K key(){
			return this.key;
		}
	
		public V value(){
			return this.value;
		}
	
		public int hashCode(){
			int hashcode = 0;
			if(this.key != null){
				hashcode += this.key.hashCode();
			}
			if(this.value != null){
				hashcode += this.value.hashCode();
			}
			return hashcode;
		}
	
		public boolean equals(Object other){
			if(this == other){
				return true;
			}
	
			if(!(other instanceof Pair)){
				// System.out.println("not of instance pair");
				return false;
			}
	
			Pair<?,?> parsed = (Pair<?,?>)other;

			if(!this.key.equals(parsed.key)){
				return false;
			}

			if(this.value.equals(SymbolType.ANY) || parsed.value.equals(SymbolType.ANY)){
				return true;
			}

			return this.value.equals(parsed.value);
		}

		public String toString(){
			return "(" + (this.key != null ? this.key.toString() : "null") + ", " + (this.value != null ? this.value.toString() : "null") + ")";
		}
	}
}
