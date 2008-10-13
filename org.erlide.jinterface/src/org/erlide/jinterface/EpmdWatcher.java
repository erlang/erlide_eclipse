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
package org.erlide.jinterface;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.ericsson.otp.erlang.OtpEpmd;

/**
 * Periodically, query epmd to see if there are any new nodes that have been
 * registered.
 * 
 */
public class EpmdWatcher {

	public EpmdWatcher() {
		try {
			addHost(InetAddress.getLocalHost().getHostName());
		} catch (UnknownHostException e) {
			addHost("localhost");
		}
	}

	private List<String> hosts = new ArrayList<String>();
	private Map<String, List<String>> nodeMap = new HashMap<String, List<String>>();
	private List<IEpmdListener> listeners = new ArrayList<IEpmdListener>();
	private Map<String, List<IEpmdMonitor>> monitors = new HashMap<String, List<IEpmdMonitor>>();
	private boolean epmdStarted = false;

	synchronized public void addHost(String host) {
		if (hosts.contains(host)) {
			return;
		}
		hosts.add(host);
		nodeMap.put(host, new ArrayList<String>());
	}

	synchronized public void removeHost(String host) {
		hosts.remove(host);
		nodeMap.remove(host);
	}

	public synchronized void checkEpmd() {
		for (Entry<String, List<String>> entry : nodeMap.entrySet()) {
			try {
				String host = entry.getKey();
				List<String> nodes = entry.getValue();

				final String[] names = OtpEpmd.lookupNames(InetAddress
						.getByName(host));
				final List<String> labels = clean(Arrays.asList(names));

				List<String> started = getDiff(labels, nodes);
				List<String> stopped = getDiff(nodes, labels);

				if (started.size() > 0 || stopped.size() > 0) {
					for (IEpmdListener listener : listeners) {
						listener.updateBackendStatus(host, started, stopped);
					}
					for (String s : started) {
						List<IEpmdMonitor> ms = monitors.get(s);
						if (ms != null) {
							for (IEpmdMonitor m : ms) {
								m.nodeUp(s);
							}
						}
					}
					for (String s : stopped) {
						List<IEpmdMonitor> ms = monitors.get(s);
						if (ms != null) {
							for (IEpmdMonitor m : ms) {
								m.nodeDown(s);
							}
						}
					}
				}

				entry.setValue(labels);
				epmdStarted = true;
			} catch (final IOException e) {
				if (epmdStarted) {
					InterfacePlugin
							.getDefault()
							.getLog()
							.log(
									new Status(IStatus.WARNING,
											InterfacePlugin.PLUGIN_ID,
											"Erlide warning: epmd daemon went down..."));
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
	public void addEpmdListener(IEpmdListener listener) {
		if (!listeners.contains(listener)) {
			listeners.add(listener);
		}
	}

	/**
	 * Unregister interest in all changes of node status
	 * 
	 * @param listener
	 */
	public void removeEpmdListener(IEpmdListener listener) {
		listeners.remove(listener);
	}

	public static List<String> clean(List<String> list) {
		List<String> result = new ArrayList<String>();
		for (String label : list) {
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

	private List<String> getDiff(List<String> list1, List<String> list2) {
		List<String> result = new ArrayList<String>(list1);
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
	public void addMonitor(String node, IEpmdMonitor monitor) {
		List<IEpmdMonitor> mons = monitors.get(node);
		if (mons == null) {
			mons = new ArrayList<IEpmdMonitor>();
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
	public void removeMonitor(String node, IEpmdMonitor monitor) {
		List<IEpmdMonitor> mons = monitors.get(node);
		if (mons == null) {
			return;
		}
		if (mons.contains(monitor)) {
			mons.remove(monitor);
			monitors.put(node, mons);
		}
	}
}
