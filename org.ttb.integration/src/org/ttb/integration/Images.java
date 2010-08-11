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
    CALL_NODE("treeviewer/callNode.png"), //
    EXIT_NODE("treeviewer/exitNode.png"), //
    FUNCTION_NODE("treeviewer/functionNode.png"), //
    GETTING_LINKED_NODE("treeviewer/gettingLinkedNode.png"), //
    GETTING_UNLINKED_NODE("treeviewer/gettingUnlinkedNode.png"), //
    IN_NODE("treeviewer/inNode.png"), //
    INFO_NODE("treeviewer/infoNode.png"), //
    LINK_NODE("treeviewer/linkNode.png"), //
    MESSAGE_NODE("treeviewer/messageNode.png"), //
    NEW_PROCESS_NODE("treeviewer/newProcessNode.png"), //
    OUT_NODE("treeviewer/outNode.png"), //
    PROCESS_NODE("treeviewer/processNode.png"), //
    RECEIVED_MESSAGE_NODE("treeviewer/receivedMessageNode.png"), //
    RECEIVER_NODE("treeviewer/receiverNode.png"), //
    REGISTER_NODE("treeviewer/registerNode.png"), //
    RETURN_FROM_NODE("treeviewer/returnFromNode.png"), //
    RETURN_TO_NODE("treeviewer/returnToNode.png"), //
    ROOT_NODE("treeviewer/rootNode.png"), //
    SENDER_NODE("treeviewer/senderNode.png"), //
    SENT_MESSAGE_NODE("treeviewer/sentMessageNode.png"), //
    SPAWN_NODE("treeviewer/spawnNode.png"), //
    TEXT_NODE("treeviewer/textNode.png"), //
    ULINK_NODE("treeviewer/ulinkNode.png"), //
    UNREGISTER_NODE("treeviewer/unregisterNode.png"), //
    WRONG_MESSAGE_NODE("treeviewer/wrongMessageNode.png");

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
