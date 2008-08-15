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
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.internal.AbstractBackend;
import org.erlide.runtime.backend.internal.ManagedBackend;

public final class BackendManager implements IResourceChangeListener {

	private static final BackendManager MANAGER = new BackendManager();

	private IdeBackend fLocalBackend;
	private final Map<String, IBackend> fBuildBackends;
	private final Object fProjectBackendsLock = new Object();
	protected List<IBackendListener> fListeners;
	private final List<ICodeBundle> fPlugins;

	// private final Object fExternalBackendsLock = new Object();

	public enum BackendEvent {
		ADDED, REMOVED
	};

	// private static final String DEFAULT_BACKEND_LABEL = "erlide";
	public static final String JAVA_NODE_LABEL = "jerlide";

	// TODO this smells bad!
	private static String fUniqueId;

	private BackendManager() {
		fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFF);

		fLocalBackend = null;
		fBuildBackends = new HashMap<String, IBackend>(5);
		fListeners = new ArrayList<IBackendListener>(5);
		fPlugins = new ArrayList<ICodeBundle>(5);

		ResourcesPlugin.getWorkspace().addResourceChangeListener(
				this,
				IResourceChangeEvent.PRE_CLOSE
						| IResourceChangeEvent.PRE_DELETE
						| IResourceChangeEvent.POST_CHANGE);

		final EpmdWatchJob job = new EpmdWatchJob();
		job.schedule(100);
	}

	public static BackendManager getDefault() {
		return MANAGER;
	}

	public enum BackendOptions {
		DEBUG, MANAGED
	};

	public IBackend create(RuntimeInfo info, Set<BackendOptions> options) {
		ErlLogger.debug("create " + options + " backend '" + info + "' "
				+ Thread.currentThread());

		final AbstractBackend b = new ManagedBackend(info);

		if (options.contains(BackendOptions.MANAGED)) {
			b.initializeRuntime();
			b.connectAndRegister(fPlugins);
			b.initErlang();
		}
		return b;
	}

	private IBackend get(final IProject project) {
		synchronized (fProjectBackendsLock) {
			// ErlLogger.debug("** getBackend: " + project.getName() + " "
			// + Thread.currentThread());

			final RuntimeInfo info = getBackendInfo(project);
			if (info == null) {
				return fLocalBackend;
			}
			IBackend b = fBuildBackends.get(info.getName());
			if (b != null && !b.ping()) {
				fBuildBackends.remove(info.getName());
				fireUpdate(b, BackendEvent.REMOVED);
				b = null;
			}
			if (b == null) {
				EnumSet<BackendOptions> options = info.isManaged() ? EnumSet
						.of(BackendOptions.MANAGED) : EnumSet
						.noneOf(BackendOptions.class);
				b = create(info, options);
				fBuildBackends.put(info.getName(), b);
				fireUpdate(b, BackendEvent.ADDED);
			}
			return b;
		}
	}

	public BuildBackend getBuild(final IProject project) {
		return get(project).asBuild();
	}

	public ExecutionBackend getExecution(final IProject project) {

		ErlLogger.error("FIXME FIXME FIXME!!!");

		return get(project).asExecution();
	}

	public static RuntimeInfo getBackendInfo(IProject project) {
		if (project == null) {
			return null;
		}
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		return prefs.getRuntimeInfo();
		// return "project";
	}

	public void remove(final IProject project) {
		/*
		 * final String name = getBackendName(project); // TODO if (not used
		 * anymore) fLocalBackends.remove(name);
		 * fProjectBackendMap.remove(project);
		 */
	}

	public synchronized IdeBackend getIdeBackend() {
		// ErlLogger.debug("** getIdeBackend: " + this + " " + fLocalBackend + "
		// "
		// + Thread.currentThread());
		// Thread.dumpStack();
		if (fLocalBackend == null) {
			ErlLogger.debug("** create InternalBackend: " + this + " "
					+ fLocalBackend + " " + Thread.currentThread());
			fLocalBackend = create(
					RuntimeInfoManager.getDefault().getErlideRuntime(),
					EnumSet.of(BackendOptions.MANAGED)).asIDE();
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

	public AbstractBackend[] getProjectBackends() {
		// getDefaultBackend();
		synchronized (fProjectBackendsLock) {
			final Object[] ob = fBuildBackends.values().toArray();
			final AbstractBackend[] res = new AbstractBackend[ob.length];
			System.arraycopy(ob, 0, res, 0, ob.length);
			return res;
		}
	}

	public void register(final ICodeBundle p) {
		if (fPlugins.indexOf(p) < 0) {
			fPlugins.add(p);
			getIdeBackend().getCodeManager().register(p);
			forEachProjectBackend(new IBackendVisitor() {
				public void run(final IBackend b) {
					b.getCodeManager().register(p);
				}
			});
		}
	}

	public void removePlugin(final ICodeBundle p) {
		fPlugins.remove(p);
		getIdeBackend().getCodeManager().unregister(p);
		forEachProjectBackend(new IBackendVisitor() {
			public void run(final IBackend b) {
				b.getCodeManager().unregister(p);
			}
		});
	}

	public void forEachProjectBackend(final IBackendVisitor visitor) {
		synchronized (fProjectBackendsLock) {
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
		return buildNodeLabel(label) + "@" + host;
	}

	public static String buildNodeLabel(final String label) {
		if (label.indexOf('_') < 0 && label.indexOf('@') < 0) {
			return label + "_" + fUniqueId;
		}
		return label;
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

	synchronized public void setEpmdStatus(List<String> started,
			List<String> stopped) {
		// TODO for started: make corresponding backend (if any) available
		for (String b : started) {
			System.out.println("started: " + b);
			IBackend bb = null;
		}
		// TODO for stopped: make corresponding backend (if any) unavailable
		for (String b : stopped) {
			System.out.println("stopped: " + b);
		}

	}
}
