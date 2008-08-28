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
package org.erlide.runtime.backend;

import java.io.IOException;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.internal.AbstractBackend;
import org.erlide.runtime.backend.internal.ManagedBackend;
import org.erlide.runtime.backend.internal.StandaloneBackend;

import com.ericsson.otp.erlang.OtpEpmd;
import com.ericsson.otp.erlang.OtpNodeStatus;

public final class BackendManager implements IResourceChangeListener {

	private static BackendManager MANAGER;

	private IdeBackend fLocalBackend;
	private final Map<IProject, BuildBackend> fBuildBackends;
	private final Object fBuildBackendsLock = new Object();
	private final Map<IProject, Set<ExecutionBackend>> fExecutionBackends;
	// private final Object fExecutionBackendsLock = new Object();
	protected List<IBackendListener> fListeners;
	private final List<ICodeBundle> fPlugins;

	// private final Object fExternalBackendsLock = new Object();

	public enum BackendEvent {
		ADDED, REMOVED
	};

	public enum BackendOptions {
		DEBUG, AUTOSTART
	};

	public static BackendManager getDefault() {
		if (MANAGER == null) {
			MANAGER = new BackendManager();
		}
		return MANAGER;
	}

	private BackendManager() {
		fLocalBackend = null;
		fBuildBackends = new HashMap<IProject, BuildBackend>();
		fExecutionBackends = new HashMap<IProject, Set<ExecutionBackend>>();
		fListeners = new ArrayList<IBackendListener>();
		fPlugins = new ArrayList<ICodeBundle>();

		ResourcesPlugin.getWorkspace().addResourceChangeListener(
				this,
				IResourceChangeEvent.PRE_CLOSE
						| IResourceChangeEvent.PRE_DELETE
						| IResourceChangeEvent.POST_CHANGE);

		final EpmdWatchJob job = new EpmdWatchJob();
		job.schedule(100);
	}

	public String getJavaNodeName() {
		String fUniqueId = getTimeSuffix();
		return "jerlide_" + fUniqueId;
	}

	private String getErlideNameSuffix() {
		String fUniqueId;
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final String location = root.getLocation().toPortableString();
		fUniqueId = Long.toHexString(location.hashCode() & 0xFFFFFFF);
		return fUniqueId;
	}

	private String getTimeSuffix() {
		String fUniqueId;
		fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFFF);
		return fUniqueId;
	}

	public IBackend create(final RuntimeInfo info,
			final Set<BackendOptions> options, ILaunch launch)
			throws BackendException {

		boolean exists = BackendManager.findRunningNode(info.getNodeName());
		AbstractBackend b = null;

		if (exists) {
			ErlLogger.debug("create standalone " + options + " backend '"
					+ info + "' " + Thread.currentThread());
			b = new StandaloneBackend(info);
		} else if (options.contains(BackendOptions.AUTOSTART)) {
			ErlLogger.debug("create managed " + options + " backend '" + info
					+ "' " + Thread.currentThread());
			b = new ManagedBackend(info);
		}
		if (b == null) {
			ErlLogger.error("Node %s not found, could not launch!", info
					.getNodeName());
			return null;
		}

		b.initializeRuntime(launch);
		b.connectAndRegister(fPlugins);
		b.initErlang();
		b.setDebug(options.contains(BackendOptions.DEBUG));

		return b;
	}

	public BuildBackend getBuild(final IProject project) {
		synchronized (fBuildBackendsLock) {
			final RuntimeInfo info = getRuntimeInfo(project);
			if (info == null) {
				ErlLogger.info("Project %s has no runtime info, using ide",
						project.getName());
				return fLocalBackend.asBuild();
			}
			final String ideName = BackendManager.getDefault().getIdeBackend()
					.getInfo().getNodeName();
			if (info.getNodeName() == null
					|| info.getNodeName().equals(ideName)
					|| info.getNodeName().equals("")) {
				return fLocalBackend.asBuild();
			}

			BuildBackend b = fBuildBackends.get(project);
			if (b != null && !b.ping()) {
				fBuildBackends.remove(info.getName());
				fireUpdate(b, BackendEvent.REMOVED);
				b = null;
			}
			if (b == null) {
				if (info.getNodeName() == null) {
					info.setNodeName(project.getName());
				}
				try {
					b = create(info, EnumSet.of(BackendOptions.AUTOSTART), null)
							.asBuild();
				} catch (BackendException e) {
					// info can't be null here
				}
				fBuildBackends.put(project, b);
				fireUpdate(b, BackendEvent.ADDED);
			}
			return b;
		}
	}

	synchronized public Set<ExecutionBackend> getExecution(
			final IProject project) {
		Set<ExecutionBackend> bs = fExecutionBackends.get(project);
		if (bs == null) {
			return Collections.emptySet();
		}
		return new HashSet<ExecutionBackend>(bs);
	}

	public static RuntimeInfo getRuntimeInfo(final IProject project) {
		if (project == null) {
			return null;
		}
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		return prefs.getRuntimeInfo();
	}

	public void remove(final IProject project) {
		/*
		 * final String name = getBackendName(project); // TODO if (not used
		 * anymore) fLocalBackends.remove(name);
		 * fProjectBackendMap.remove(project);
		 */
	}

	public synchronized IdeBackend getIdeBackend() {
		// ErlLogger.debug("** getIdeBackend: " + this + " " + fLocalBackend
		// + "		 " + Thread.currentThread());
		// Thread.dumpStack();
		if (fLocalBackend == null) {
			ErlLogger.debug("** create InternalBackend: "
					+ Thread.currentThread());

			final RuntimeInfo erlideRuntime = RuntimeInfoManager.getDefault()
					.getErlideRuntime();
			if (erlideRuntime != null) {
				try {
					erlideRuntime
							.setNodeName("erlide_" + getErlideNameSuffix());
					fLocalBackend = create(erlideRuntime,
							EnumSet.of(BackendOptions.AUTOSTART), null).asIDE();
				} catch (BackendException e) {
					// erlideRuntime can't be null here
				}
			}
		}
		return fLocalBackend;
	}

	public static String getLabelProperty() {
		return System.getProperty("erlide.label");
	}

	public static boolean isDeveloper() {
		final String dev = System.getProperty("erlide.devel");
		return dev != null && "true".equals(dev);
	}

	public static boolean isTest() {
		final String test = System.getProperty("erlide.test");
		return test != null && "true".equals(test);
	}

	public void addBackendListener(final IBackendListener listener) {
		fListeners.add(listener);
	}

	public void removeBackendListener(final IBackendListener listener) {
		fListeners.remove(listener);
	}

	private void fireUpdate(final IBackend b, final BackendEvent type) {
		new BackendChangeNotifier().notify(b, type);
	}

	/**
	 * Notifies a backend listener of additions or removals
	 */
	class BackendChangeNotifier implements ISafeRunnable {

		private IBackendListener fListener;

		private BackendEvent fType;

		private IBackend fChanged;

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.eclipse.core.runtime.ISafeRunnable#handleException(java.lang.
		 * Throwable)
		 */
		public void handleException(final Throwable exception) {
			final IStatus status = new Status(IStatus.ERROR,
					ErlangLaunchPlugin.PLUGIN_ID, 1,
					"backend listener exception", exception);
			ErlangLaunchPlugin.log(status);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.core.runtime.ISafeRunnable#run()
		 */
		public void run() throws Exception {
			switch (fType) {
			case ADDED:
				fListener.backendAdded(fChanged);
				break;
			case REMOVED:
				fListener.backendRemoved(fChanged);
				break;
			}
		}

		/**
		 * Notifies the given listener of the adds/removes
		 */
		public void notify(final IBackend b, final BackendEvent type) {
			if (fListeners == null) {
				return;
			}

			fChanged = b;
			fType = type;
			final Object[] copiedListeners = fListeners.toArray();
			for (final Object element : copiedListeners) {
				fListener = (IBackendListener) element;
				SafeRunner.run(this);
			}
			fChanged = null;
			fListener = null;
		}
	}

	public IBackend[] getAllBackends() {
		synchronized (fBuildBackendsLock) {
			final Object[] ob = fBuildBackends.values().toArray();
			final IBackend[] res = new IBackend[ob.length + 1];
			System.arraycopy(ob, 0, res, 0, ob.length);
			res[ob.length] = getIdeBackend();
			return res;
		}
	}

	public void register(final ICodeBundle p) {
		if (fPlugins.indexOf(p) < 0) {
			fPlugins.add(p);
			if (fLocalBackend != null) {
				fLocalBackend.getCodeManager().register(p);
			}
			forEachProjectBackend(new IBackendVisitor() {
				public void run(final IBackend b) {
					b.getCodeManager().register(p);
				}
			});
		}
	}

	public void removePlugin(final ICodeBundle p) {
		fPlugins.remove(p);
		fLocalBackend.getCodeManager().unregister(p);
		forEachProjectBackend(new IBackendVisitor() {
			public void run(final IBackend b) {
				b.getCodeManager().unregister(p);
			}
		});
	}

	public void forEachProjectBackend(final IBackendVisitor visitor) {
		synchronized (fBuildBackendsLock) {
			for (final IBackend b : fBuildBackends.values()) {
				try {
					visitor.run(b);
				} catch (final Exception e) {
				}
			}
		}
	}

	public static String buildNodeName(final String label) {
		if (label.indexOf('@') > 0) {
			return label;
		}
		final String host = getHost();
		return label + "@" + host;
	}

	public static String getHost() {
		String host;
		try {
			host = InetAddress.getLocalHost().getHostName();
			if (System.getProperty("erlide.host") != null) {
				final int dot = host.indexOf(".");
				if (dot != -1) {
					host = host.substring(0, dot);
				}
			}
		} catch (final IOException e) {
			host = "localhost";
			ErlangLaunchPlugin.log(e);
		}
		return host;
	}

	public void resourceChanged(final IResourceChangeEvent event) {
		// ErlLogger.debug("+BM: " + event.getType() + " " +
		// event.getResource()
		// + event.getDelta());
		switch (event.getType()) {
		case IResourceChangeEvent.PRE_CLOSE:
		case IResourceChangeEvent.PRE_DELETE:
			break;

		}
	}

	synchronized public void updateBackendStatus(final List<String> started,
			final List<String> stopped) {
		for (final String b : started) {
			ErlLogger.info("(epmd) started: '%s'", b);
			for (final IBackend bb : getAllBackends()) {
				if (bb != null) {
					((OtpNodeStatus) bb).remoteStatus(b, true, null);
				}
			}
		}
		for (final String b : stopped) {
			ErlLogger.info("(epmd) stopped: '%s'", b);
			for (final IBackend bb : getAllBackends()) {
				if (bb != null) {
					((OtpNodeStatus) bb).remoteStatus(b, false, null);
				}
			}
		}

	}

	synchronized public void addExecution(IProject project, ExecutionBackend b) {
		Set<ExecutionBackend> list = fExecutionBackends.get(project);
		if (list == null) {
			list = new HashSet<ExecutionBackend>();
			fExecutionBackends.put(project, list);
		}
		list.add(b);
	}

	synchronized public void removeExecution(IProject project,
			ExecutionBackend b) {
		Set<ExecutionBackend> list = fExecutionBackends.get(project);
		if (list == null) {
			list = new HashSet<ExecutionBackend>();
			fExecutionBackends.put(project, list);
		}
		list.remove(b);
	}

	public static boolean findRunningNode(String nodeName) {
		try {
			final String[] names = OtpEpmd.lookupNames();
			final List<String> labels = Arrays.asList(names);
			EpmdWatchJob.clean(labels);
			for (String name : labels) {
				if (name.equals(nodeName)) {
					return true;
				}
			}
		} catch (IOException e) {
		}
		return false;
	}

}
