package org.ttb.integration;

/**
 * Enum describing images used by plugin.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum Images {

    /**
     * checked ckeckbox
     */
    CHECKED("checked.gif"),
    /**
     * unchecked ckeckbox
     */
    UNCHECKED("unchecked.gif"), //
    TREE_NODE("treeNode.gif"), //
    TREE_ROOT("treeRoot.gif");

    private String fileName;

    private Images(String fileName) {
        this.fileName = fileName;
    }

    /**
     * Returns file name represented by this enum value.
     * 
     * @return file name
     */
    public String getFileName() {
        return fileName;
    }
}
