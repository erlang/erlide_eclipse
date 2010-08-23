package org.ttb.integration.mvc.model.treenodes;

import org.ttb.integration.Activator;
import org.ttb.integration.Images;

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
        setImage(Activator.getImage(Images.INFO_NODE));
    }

    public String getModuleName() {
        return moduleName;
    }
}
