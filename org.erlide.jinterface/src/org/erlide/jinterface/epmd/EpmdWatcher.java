/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.epmd;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpEpmd;

/**
 * Query epmd to see if there are any new nodes that have been registered and
 * notify listeners.
 */
public final class EpmdWatcher {

    public EpmdWatcher() {
        try {
            addHost(InetAddress.getLocalHost().getHostName());
        } catch (final UnknownHostException e) {
            addHost("localhost");
        }
    }

    private final List<String> hosts = new ArrayList<String>();
    private final Map<String, List<String>> nodeMap = new HashMap<String, List<String>>();
    private final List<IEpmdListener> listeners = new ArrayList<IEpmdListener>();
    private final Map<String, List<IErlNodeMonitor>> monitors = new HashMap<String, List<IErlNodeMonitor>>();
    private boolean epmdStarted = false;

    synchronized public void addHost(final String host) {
        if (hosts.contains(host)) {
            return;
        }
        hosts.add(host);
        nodeMap.put(host, new ArrayList<String>());
    }

    synchronized public void removeHost(final String host) {
        hosts.remove(host);
        nodeMap.remove(host);
    }

    public synchronized void checkEpmd() {
        for (final Entry<String, List<String>> entry : nodeMap.entrySet()) {
            try {
                final String host = entry.getKey();
                final List<String> nodes = entry.getValue();

                final String[] names = OtpEpmd.lookupNames(InetAddress
                        .getByName(host));
                final List<String> labels = clean(Arrays.asList(names));

                final List<String> started = getDiff(labels, nodes);
                final List<String> stopped = getDiff(nodes, labels);

                if (started.size() > 0 || stopped.size() > 0) {
                    for (final IEpmdListener listener : listeners) {
                        listener.updateNodeStatus(host, started, stopped);
                    }
                    for (final String s : started) {
                        final List<IErlNodeMonitor> ms = monitors.get(s);
                        if (ms != null) {
                            for (final IErlNodeMonitor m : ms) {
                                m.nodeUp(s);
                            }
                        }
                    }
                    for (final String s : stopped) {
                        final List<IErlNodeMonitor> ms = monitors.get(s);
                        if (ms != null) {
                            for (final IErlNodeMonitor m : ms) {
                                m.nodeDown(s);
                            }
                        }
                    }
                }

                entry.setValue(labels);
                epmdStarted = true;
            } catch (final IOException e) {
                if (epmdStarted) {
                    final String msg = "Erlide warning: epmd daemon went down on host "
                            + entry.getKey() + "...";
                    // InterfacePlugin.getDefault().getLog().log(
                    // new Status(IStatus.WARNING,
                    // InterfacePlugin.PLUGIN_ID, msg));
                    ErlLogger.warn(msg);
                    epmdStarted = false;
                }
            }
        }
    }

    /**
     * Register interest in all changes of node status
     * 
     * @param listener
     */
    public void addEpmdListener(final IEpmdListener listener) {
        if (!listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    /**
     * Unregister interest in all changes of node status
     * 
     * @param listener
     */
    public void removeEpmdListener(final IEpmdListener listener) {
        listeners.remove(listener);
    }

    public static List<String> clean(final List<String> list) {
        final List<String> result = new ArrayList<String>();
        for (final String label : list) {
            if ("".equals(label)) {
                continue;
            }
            // label is "name X at port N"
            final String[] parts = label.split(" ");
            if (parts.length == 5) {
                String alabel = parts[1];
                if (alabel.length() == 0) {
                    alabel = "??" + label;
                }
                result.add(alabel);
            }
        }
        return result;
    }

    private List<String> getDiff(final List<String> list1,
            final List<String> list2) {
        final List<String> result = new ArrayList<String>(list1);
        result.removeAll(list2);
        return result;
    }

    public Map<String, List<String>> getData() {
        return nodeMap;
    }

    /**
     * Register interest in the status of a certain node.
     * 
     * @param node
     * @param monitor
     */
    public void addNodeMonitor(final String node, final IErlNodeMonitor monitor) {
        List<IErlNodeMonitor> mons = monitors.get(node);
        if (mons == null) {
            mons = new ArrayList<IErlNodeMonitor>();
        }
        if (mons.contains(monitor)) {
            return;
        }
        mons.add(monitor);
        monitors.put(node, mons);
    }

    /**
     * Unregister interest in the status of a certain node.
     * 
     * @param node
     * @param monitor
     */
    public void removeNodeMonitor(final String node,
            final IErlNodeMonitor monitor) {
        final List<IErlNodeMonitor> mons = monitors.get(node);
        if (mons == null) {
            return;
        }
        if (mons.contains(monitor)) {
            mons.remove(monitor);
            monitors.put(node, mons);
        }
    }

    public boolean hasLocalNode(final String nodeName) {
        try {
            final String[] names = OtpEpmd.lookupNames();
            final List<String> labels = EpmdWatcher.clean(Arrays.asList(names));
            return labels.contains(nodeName);
        } catch (final IOException e) {
            return false;
        }
    }

}
