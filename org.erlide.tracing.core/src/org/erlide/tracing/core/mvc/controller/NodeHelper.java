package org.erlide.tracing.core.mvc.controller;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
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
    public static boolean isExternal(final String nodeName) {
        for (final IBackend backend : getBackends(false)) {
            if (backend.getFullNodeName().equals(nodeName)) {
                return false;
            }
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
    public static Collection<? extends IBackend> getBackends(
            final boolean ignore) {
        if (!ignore) {
            return BackendCore.getBackendManager().getAllBackends();
        }

        final List<IBackend> backends = new ArrayList<IBackend>();
        final IBackendManager backendManager = BackendCore.getBackendManager();
        final Set<IBackend> ignored = new HashSet<IBackend>();
        IBackend backend;

        if ((backend = backendManager.getIdeBackend()) != null) {
            ignored.add(backend);
        }
        if ((backend = TraceBackend.getInstance().getBackend(false)) != null) {
            ignored.add(backend);
        }
        for (final IBackend erlideBackend : BackendCore.getBackendManager()
                .getAllBackends()) {
            if (!ignored.contains(erlideBackend)) {
                backends.add(erlideBackend);
            }
        }
        return backends;
    }
}
