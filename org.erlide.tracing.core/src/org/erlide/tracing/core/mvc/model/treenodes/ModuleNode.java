package org.erlide.tracing.core.mvc.model.treenodes;

import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.Images;

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
    public ModuleNode(final String moduleName) {
        this.moduleName = moduleName;
        setImage(Activator.getImage(Images.INFO_NODE));
    }

    public String getModuleName() {
        return moduleName;
    }
}
