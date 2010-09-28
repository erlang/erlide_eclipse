package org.ttb.integration.mvc.model.treenodes;

import org.ttb.integration.Activator;
import org.ttb.integration.Images;

/**
 * Node representing function in treeviewer.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class FunctionNode extends TreeNode {
    private final String moduleName;
    private final String functionName;
    private final int arity;

    /**
     * Creates new node.
     * 
     * @param moduleName
     *            module name
     * @param functionName
     *            function name
     * @param arity
     *            function arity
     */
    public FunctionNode(String moduleName, String functionName, int arity) {
        this.moduleName = moduleName;
        this.functionName = functionName;
        this.arity = arity;
        setImage(Activator.getImage(Images.INFO_NODE));
    }

    public String getModuleName() {
        return moduleName;
    }

    public String getFunctionName() {
        return functionName;
    }

    public int getArity() {
        return arity;
    }
}
