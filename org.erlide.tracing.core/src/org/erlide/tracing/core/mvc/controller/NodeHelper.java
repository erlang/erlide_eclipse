package org.erlide.tracing.core.mvc.controller;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.erlide.jinterface.backend.Backend;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErlideBackend;
import org.erlide.tracing.core.TraceBackend;

/**
 * Helper class containing methods for dealing with traced nodes.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class NodeHelper {

    private NodeHelper() {
    }

    /**
     * Checks if node with given name was started from erlide or was started
     * outside it (in this case it is considered as <i>external</i>).
     * 
     * @param nodeName
     *            node name
     * @return <code>true</code> if node was started outside erlide,
     *         <code>false</code> otherwise
     */
    public static boolean isExternal(String nodeName) {
        for (Backend backend : getBackends(false)) {
            if (backend.getPeer().equals(nodeName))
                return false;
        }
        return true;
    }

    /**
     * Returns backends managed by erlide. Depending on argument nodes
     * irrelevant from user's point of view (tracing and ide backends) can be
     * omitted in resulting list.
     * 
     * @param ignore
     *            if nodes should be omitted
     * @return list of backends
     */
    public static Collection<? extends Backend> getBackends(boolean ignore) {
        if (!ignore)
            return BackendManager.getDefault().getAllBackends();

        List<Backend> backends = new ArrayList<Backend>();
        BackendManager backendManager = BackendManager.getDefault();
        Set<Backend> ignored = new HashSet<Backend>();
        Backend backend;

        if ((backend = backendManager.getIdeBackend()) != null)
            ignored.add(backend);
        if ((backend = TraceBackend.getInstance().getBackend(false)) != null)
            ignored.add(backend);
        for (ErlideBackend erlideBackend : BackendManager.getDefault().getAllBackends()) {
            if (!ignored.contains(erlideBackend))
                backends.add(erlideBackend);
        }
        return backends;
    }
}
