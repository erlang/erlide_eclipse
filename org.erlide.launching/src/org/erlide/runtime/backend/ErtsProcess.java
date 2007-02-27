/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.erlide.basiccore.ErtsPreferences;

/**
 * 
 * 
 * @author Vlad Dumitrescu [vlad_dumitrescu at hotmail dot com]
 */
public class ErtsProcess extends RuntimeProcess {

	private ErtsPreferences fConfiguration;

	@SuppressWarnings("unused")
	private final Map fAttributes = new HashMap();

	public ErtsProcess(ILaunch launch, Process process, String name,
			Map attributes) {
		super(launch, process, name, attributes);
		// System.out.println("# create ErtsNode: " + name + " " + attributes);
	}

	public static final String CONFIGURATION_TYPE = "org.erlide.core.launch.erlangProcess";

	/**
	 * @return Returns the started.
	 */
	public boolean isStarted() {
		return getLaunch() != null;
	}

	/**
	 * @return Returns the configuration.
	 */
	public ErtsPreferences getConfiguration() {
		return fConfiguration;
	}

	public void setConfiguration(ErtsPreferences configuration) {
		fConfiguration = configuration;
	}

	/**
	 * Write something out to the node process.
	 * 
	 * @param value
	 *            The system.
	 * @throws IOException
	 */
	public synchronized void writeToErlang(String value) throws IOException {
		if (!isStarted()) {
			return;
		}
		final IStreamsProxy streamsProxy = getLaunch().getProcesses()[0]
				.getStreamsProxy();
		if (streamsProxy != null) {
			streamsProxy.write(value);
		}
	}

	/**
	 * if this isn't already stopped, try to stop it.
	 * 
	 * @throws Throwable
	 */
	@Override
	protected void finalize() throws Throwable {
		terminate();
		super.finalize();
	}

	@SuppressWarnings("unused")
	private String[] mkEnv(Map<String, String> map) {
		final Set<Map.Entry<String, String>> entries = map.entrySet();
		final String[] result = new String[entries.size()];
		final Iterator<Map.Entry<String, String>> i = entries.iterator();
		int ii = 0;
		while (i.hasNext()) {
			final Map.Entry<String, String> e = i.next();
			result[ii++] = e.getKey() + "=" + e.getValue();
		}
		return result;
	}

	public void addStdListener(IStreamListener dspHandler) {
		final IStreamsProxy streamsProxy = getStreamsProxy();
		if (streamsProxy != null) {
			streamsProxy.getOutputStreamMonitor().addListener(dspHandler);
		}
	}

	public void addErrListener(IStreamListener errHandler) {
		final IStreamsProxy streamsProxy = getStreamsProxy();
		if (streamsProxy != null) {
			streamsProxy.getErrorStreamMonitor().addListener(errHandler);
		}
	}

	public void sendToShell(String string) {
		final IStreamsProxy streamsProxy = getStreamsProxy();
		if (streamsProxy != null) {
			try {
				streamsProxy.write(string);
				System.out.println("#>>#" + string);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

	}

}
