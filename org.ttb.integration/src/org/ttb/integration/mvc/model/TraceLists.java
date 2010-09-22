package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.ttb.integration.mvc.model.treenodes.ITreeNode;

/**
 * Class that contains lists used as models.
 * 
 * @author Piotr Dorobisz
 * 
 */
public final class TraceLists {

    private TraceLists() {
    }

    private static final List<ITreeNode> filesList = Collections.synchronizedList(new ArrayList<ITreeNode>());
    private static final List<ITreeNode> tracesList = Collections.synchronizedList(new ArrayList<ITreeNode>());

    /**
     * Returns list of nodes that represents files with tracing results.
     * 
     * @return list
     */
    public static List<ITreeNode> getFilesList() {
        return filesList;
    }

    /**
     * Returns list of nodes that represents trace events.
     * 
     * @return list
     */
    public static List<ITreeNode> getTracesList() {
        return tracesList;
    }
}
