package org.ttb.integration.mvc.model.treenodes;

/**
 * Node representing module in treeviewer.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ModuleNode extends TreeNode {

    private final String moduleName;

    /**
     * Creates new node.
     * 
     * @param moduleName
     *            module name
     */
    public ModuleNode(String moduleName) {
        this.moduleName = moduleName;
    }

    public String getModuleName() {
        return moduleName;
    }
}
