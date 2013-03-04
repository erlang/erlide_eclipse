package org.erlide.runtime.epmd;

import java.util.List;
import java.util.Map;

public interface IEpmdWatcher {

    public abstract void addHost(String host);

    public abstract void removeHost(String host);

    public abstract void checkEpmd();

    /**
     * Register interest in all changes of node status
     * 
     * @param listener
     */
    public abstract void addEpmdListener(IEpmdListener listener);

    /**
     * Unregister interest in all changes of node status
     * 
     * @param listener
     */
    public abstract void removeEpmdListener(IEpmdListener listener);

    public abstract Map<String, List<String>> getData();

    /**
     * Register interest in the status of a certain node.
     * 
     * @param node
     * @param monitor
     */
    public abstract void addNodeMonitor(String node, IErlNodeMonitor monitor);

    /**
     * Unregister interest in the status of a certain node.
     * 
     * @param node
     * @param monitor
     */
    public abstract void removeNodeMonitor(String node, IErlNodeMonitor monitor);

    public abstract boolean hasLocalNode(String nodeName);

}
