package staticTypechecker.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;
import java.util.Map.Entry;

import jolie.lang.NativeType;
import jolie.lang.parse.ast.types.BasicTypeDefinition;
import jolie.util.Pair;
import staticTypechecker.entities.Path;
import staticTypechecker.entities.ChoiceType;
import staticTypechecker.entities.InlineType;
import staticTypechecker.entities.Type;

/**
 * Utily functions for working with type trees.
 * 
 * @author Kasper Bergstedt (kasper.bergstedt@hotmail.com)
 */
public class TypeUtils {
	/**
	 * Finds the exact nodes at the specified path. That is, if it is a choice node at the end of the path, the choice node is returned, rather than the different choices as in the findNodes function.
	 * @param path the path to follow
	 * @param root the root of the tree to search
	 * @param createPath whether the path should be created with void nodes if it is not present
	 * @return an arraylist of the nodes found at the end of the path
	 */
	public static ArrayList<Type> findNodesExact(Path path, Type root, boolean createPath, boolean unfold){
		ArrayList<Type> ret = new ArrayList<>();
		ArrayList<Pair<InlineType, String>> parents = TypeUtils.findParentAndName(path, root, createPath, true);

		for(Pair<InlineType, String> pair : parents){
			InlineType parent = pair.key();
			Type child = parent.getChild(pair.value());
			ret.add(child);
		}

		return ret;
	}

	/**
	 * Finds the parent nodes and child names of the nodes at the specified path. I.e. if the provided path is a.b.c, then it would return the pair (b, "c"), since b is the parent node and the childname is c.
	 * @param path the path to follow
	 * @param root the root node of the tree
	 * @param createPath whether or not the path should be created with void instances, if it does not exist
	 * @param unfold whether or not to unfold the structure in case of a recursive type structure
	 * @return an ArrayList of Pairs of InlineTypes and Strings, indicating the parent nodes and the child names
	 */
	public static ArrayList<Pair<InlineType, String>> findParentAndName(Path path, Type root, boolean createPath, boolean createChild){
		if(root instanceof InlineType){
			return TypeUtils.findParentAndNameRec(path, (InlineType)root, createPath, createChild, Collections.newSetFromMap(new IdentityHashMap<>()));
		}
		else{ // root is ChoiceType
			ArrayList<Pair<InlineType, String>> result = new ArrayList<>();
			for(InlineType choice : ((ChoiceType)root).choices()){
				result.addAll(TypeUtils.findParentAndNameRec(path, choice, createPath, createChild, Collections.newSetFromMap(new IdentityHashMap<>())));
			}

			return result;
		}
	}

	private static ArrayList<Pair<InlineType, String>> findParentAndName(Path path, Type root, boolean createPath){
		return TypeUtils.findParentAndName(path, root, createPath, true);
	}

	private static ArrayList<Pair<InlineType, String>> findParentAndNameRec(Path path, InlineType root, boolean createPath, boolean createChild, Set<InlineType> seenNodes){
		ArrayList<Pair<InlineType, String>> ret = new ArrayList<>();
		seenNodes.add(root);
				
		if(path.isEmpty()){
			return ret;
		}

		String childNameToLookFor = path.get(0);
		Type childNode;

		if(root.contains(childNameToLookFor)){
			childNode = root.getChild(childNameToLookFor);

			if(path.size() == 1){ // it was the child to look for
				ret.add(new Pair<InlineType, String>(root, childNameToLookFor));
				return ret;
			}
			
			if(childNode instanceof InlineType){ 
				InlineType parsedChild = (InlineType)childNode;
				ret.addAll(TypeUtils.findParentAndNameRec(path.remainder(), parsedChild, createPath, createChild, seenNodes));
			}
			else{ // child is choice node, continue search in all choices
				ChoiceType parsedChild = (ChoiceType)childNode;
				parsedChild.choices().forEach(c -> ret.addAll(TypeUtils.findParentAndNameRec(path.remainder(), c, createPath, createChild, seenNodes)));
			}
		}
		else if(createPath){
			InlineType newChild = new InlineType(BasicTypeDefinition.of(NativeType.VOID), null, null, false);
			
			if(path.size() == 1){ // this was the child to look for
				ret.add(new Pair<InlineType,String>(root, childNameToLookFor));

				if(createChild){
					root.addChildUnsafe(childNameToLookFor, newChild);
				}
			}
			else{
				root.addChildUnsafe(childNameToLookFor, newChild);
				ret.addAll(TypeUtils.findParentAndNameRec(path.remainder(), newChild, createPath, createChild, seenNodes));
			}
		}

		return ret;
	}

	/**
	 * Unfolds the Type root in place.
	 * @param path the path to follow. The unfolding will stop when reaching the end of this path.
	 * @param root the Type to unfold.
	 * @param rootCopy a deep copy of root.
	 * @param seenNodes a Set<Type> of the nodes that already have been processed.
	 */
	public static Type unfold(Path path, Type root){
		return TypeUtils.unfoldRec(path, root.copy(), root.copy(), Collections.newSetFromMap(new IdentityHashMap<>()));
	}
	
	private static Type unfoldRec(Path path, Type root, Type rootCopy, Set<Type> seenNodes){
		seenNodes.add(root);
		
		if(root instanceof InlineType){
			InlineType parsedRoot = (InlineType)root;
			InlineType parsedRootCopy = (InlineType)rootCopy;

			if(path.isEmpty()){ // we do not need to unfold anymore, only update the children
				for(Entry<String, Type> ent : parsedRootCopy.children().entrySet()){
					parsedRoot.addChildUnsafe(ent.getKey(), ent.getValue());
				}
				return root;
			}
			else{
				String childToLookFor = path.get(0);

				for(String childName : parsedRoot.children().keySet()){
					Type child = parsedRoot.getChild(childName);
					Type childCopy = parsedRootCopy.getChild(childName);

					if(childName.equals(childToLookFor)){ // it is the next child on the path
						if(seenNodes.contains(child)){ // a recursive edge
							// unfold once
							child = childCopy.copy();
							parsedRoot.addChildUnsafe(childName, child);
						}

						// continue on the path
						TypeUtils.unfoldRec(path.remainder(), child, childCopy, seenNodes);
					}
					else{ // it is not a child on the path
						if(seenNodes.contains(child)){ // a recursive edge
							// point to the copy structure
							parsedRoot.addChildUnsafe(childName, childCopy);
						}
					}
				}
			}
		}
		else{ // choice type
			ChoiceType parsedRoot = (ChoiceType)root;
			ChoiceType parsedRootCopy = (ChoiceType)rootCopy;

			// for each choice in the root find the equivalent in the copy root and continue the unfolding there
			for(InlineType choice : parsedRoot.choices()){
				for(InlineType choiceCopy : parsedRootCopy.choices()){
					if(choice.equals(choiceCopy)){
						TypeUtils.unfoldRec(path, choice, choiceCopy, seenNodes);
						break;
					}
				}
			}
		}

		return root;
	}

	/**
	 * Sets the type of the nodes at the end of the specified path by the given type.
	 * @param path the path to follow.
	 * @param type the type to update all nodes at the end of the path to.
	 * @param tree the tree in which the nodes reside.
	 */
	public static void setTypeOfNodeByPath(Path path, Type type, Type tree){
		ArrayList<Pair<InlineType,String>> nodesToUpdate = TypeUtils.findParentAndName(path, tree, true, true);
		
		for(Pair<InlineType,String> pair : nodesToUpdate){
			pair.key().addChildUnsafe(pair.value(), type);
		}
	}

	/**
	 * Updates the basic type of the nodes specified by the given path. NOTE: unfolds the type in case of a recursive type.
	 * @param path the path to follow.
	 * @param basicTypes the new basic types.
	 * @param tree the root of the tree in which to follow the path.
	 */
	public static void setBasicTypeOfNodeByPath(Path path, List<BasicTypeDefinition> basicTypes, Type tree){
		TypeUtils.unfold(path, tree);

		ArrayList<Pair<InlineType,String>> nodesToUpdate = TypeUtils.findParentAndName(path, tree, true, true);
		
		for(Pair<InlineType,String> pair : nodesToUpdate){
			InlineType parent = pair.key();
			String childName = pair.value();

			if(basicTypes.size() == 1){ // only one basic type, update the child in the parent with a copy of the old child with a new basic type
				if(parent.getChild(childName) instanceof InlineType){
					parent.addChildUnsafe(childName, ((InlineType)parent.getChild(childName)).setBasicType(basicTypes.get(0)));
				}
				else{
					ChoiceType child = ((ChoiceType)parent.getChild(childName)).updateBasicTypeOfChoices(basicTypes.get(0));
					// this check is necessary, since changing the basic type of all choices, may make some of them equivalent, and hence they will be removed. We may be in a situation of a choice type with only one choice (which we convert to an InlineType below)
					if(child.choices().size() == 1){
						parent.addChildUnsafe(childName, new InlineType(basicTypes.get(0), null, child.context(), false));
					}
					else{
						parent.addChildUnsafe(childName, child);
					}
				}
			}
			else{ // more than one basic type
				ChoiceType newChild = new ChoiceType();

				if(parent.getChild(childName) instanceof InlineType){
					InlineType oldChild = (InlineType)parent.getChild(childName);

					for(BasicTypeDefinition b : basicTypes){
						newChild.addChoiceUnsafe(oldChild.setBasicType(b));
					}
				}
				else{
					ChoiceType oldChild = (ChoiceType)parent.getChild(childName);
					
					for(BasicTypeDefinition b : basicTypes){
						for(InlineType c : oldChild.choices()){
							newChild.addChoiceUnsafe(c.setBasicType(b));
						}
					}
				}

				parent.addChildUnsafe(childName, newChild);
			}
		}
	}

	/**
	 * Changes the nodes at the end of the given Paths to any{?}.
	 * @param T0 the tree in which to follow the Paths. NOTE: this tree is not altered.
	 * @param paths the paths to follow.
	 * @return a copy of T0 with the paths undefined.
	 */
	public static Type undefine(Type T0, ArrayList<Path> paths){
		Type copy = T0.copy();
		
		for(Path path : paths){
			ArrayList<Pair<InlineType, String>> nodesToUndefine = TypeUtils.findParentAndName(path, copy, true);
			
			for(Pair<InlineType, String> p : nodesToUndefine){
				InlineType parent = p.key();
				String childName = p.value();

				parent.addChildUnsafe(childName, Type.UNDEFINED());
			}
		}

		return copy;
	}
}
