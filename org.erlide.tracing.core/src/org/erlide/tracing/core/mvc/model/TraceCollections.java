package org.erlide.tracing.core.mvc.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.TreeSet;

import org.erlide.tracing.core.mvc.model.treenodes.ITreeNode;

/**
 * Class that contains collections used as models.
 * 
 * @author Piotr Dorobisz
 * 
 */
public final class TraceCollections {

    private TraceCollections() {
    }

    private static final Collection<ITreeNode> filesList = Collections
            .synchronizedSortedSet(new TreeSet<ITreeNode>());
    private static final Collection<ITreeNode> tracesList = Collections
            .synchronizedList(new ArrayList<ITreeNode>());

    /**
     * Returns collection of nodes that represents files with tracing results.
     * 
     * @return list
     */
    public static Collection<ITreeNode> getFilesList() {
        return filesList;
    }

    /**
     * Returns collection of nodes that represents trace events.
     * 
     * @return list
     */
    public static Collection<ITreeNode> getTracesList() {
        return tracesList;
    }
}
