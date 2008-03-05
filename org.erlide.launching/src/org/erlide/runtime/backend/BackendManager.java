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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.internal.AbstractBackend;
import org.erlide.runtime.backend.internal.ManagedBackend;
import org.erlide.runtime.backend.internal.StandaloneBackend;
import org.erlide.runtime.debug.ErlangDebugTarget;

public final class BackendManager implements IResourceChangeListener {

	private static final BackendManager MANAGER = new BackendManager();

	private IBackend fLocalBackend;

	private final Map<String, IBackend> fProjectBackends;

	private final Object fProjectBackendsLock = new Object();

	private final IBackend fRemoteBackend;

	protected List<IBackendListener> fListeners;

	private final List<ICodeBundle> fPlugins;

	private final Map<String, IBackend> fExternalBackends = new HashMap<String, IBackend>(
			2);

	// private final Object fExternalBackendsLock = new Object();

	private static final int ADDED = 1;

	private static final int REMOVED = 2;

	private static final String DEFAULT_BACKEND_LABEL = "erlide";

	public static final String JAVA_NODE_LABEL = "jerlide";

	// TODO this smells bad!
	private static String fUniqueId;

	private BackendManager() {
		fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFF);

		fLocalBackend = null;
		fRemoteBackend = null;
		fProjectBackends = new HashMap<String, IBackend>(5);
		fListeners = new ArrayList<IBackendListener>(5);
		fPlugins = new ArrayList<ICodeBundle>(5);

		ResourcesPlugin.getWorkspace().addResourceChangeListener(
				this,
				IResourceChangeEvent.PRE_CLOSE
						| IResourceChangeEvent.PRE_DELETE
						| IResourceChangeEvent.POST_CHANGE);
	}

	public static BackendManager getDefault() {
		return MANAGER;
	}

	public IBackend createManaged(String name, boolean debug) {
		ErlLogger.debug("create managed backend '" + name + "'.");

		final AbstractBackend b = new ManagedBackend();
		b.setLabel(name);

		final ILaunch launch = b.initialize();
		b.connect();
		if (debug) {
			final IDebugTarget target = new ErlangDebugTarget(launch, b, "", "");
			launch.addDebugTarget(target);
		}

		for (final ICodeBundle element : fPlugins) {
			b.getCodeManager().register(element);
		}

		b.init_erlang();

		return b;
	}

	public IBackend createStandalone(String name) {
		ErlLogger.debug("create standalone backend " + name + ".");

		final AbstractBackend b = new StandaloneBackend();
		b.setLabel(name);

		final ILaunch launch = b.initialize();
		b.connect();
		if (launch != null) {
			final IDebugTarget target = new ErlangDebugTarget(launch, b, "", "");
			launch.addDebugTarget(target);
		}

		for (final ICodeBundle element : fPlugins) {
			b.getCodeManager().register(element);
		}
		return b;
	}

	public IBackend get(IProject project) {
		synchronized (fProjectBackendsLock) {

			final String name = getBackendName(project);
			if (name.equals(DEFAULT_BACKEND_LABEL)) {
				return fLocalBackend;
			}
			IBackend b = fProjectBackends.get(name);
			if (b != null && !b.ping()) {
				fProjectBackends.remove(name);
				b = null;
			}
			if (b == null) {
				b = createManaged(name, false);
				fProjectBackends.put(name, b);
				fireUpdate(b, ADDED);
			}
			return b;
		}
	}

	public IBackend getExternal(String nodeName) {
		// synchronized (fExternalBackendsLock) {
		final IBackend b = fExternalBackends.get(nodeName);
		return b;
		// }
	}

	public IBackend createExternal(String nodeName, String cookie) {
		final AbstractBackend b = new StandaloneBackend();
		b.setLabel(nodeName);
		b.connect(cookie);
		// for (final ICodeBundle element : fPlugins) {
		// b.getCodeManager().register(element);
		// }
		fExternalBackends.put(nodeName, b);
		return b;

	}

	public static String getBackendName(IProject project) {
		if (project == null) {
			return DEFAULT_BACKEND_LABEL;
		}
		final ErlangProjectProperties prefs = new ErlangProjectProperties(
				project);
		final String prjLabel = prefs.getBackendName();
		if (prjLabel.length() == 0) {
			// we use the ide backend in this case
			return DEFAULT_BACKEND_LABEL;
		}
		return prjLabel;
	}

	public void remove(IProject project) {
		/*
		 * final String name = getBackendName(project); // TODO if (not used
		 * anymore) fLocalBackends.remove(name);
		 * fProjectBackendMap.remove(project);
		 */
	}

	public IBackend getIdeBackend() {
		if (fLocalBackend == null) {
			fLocalBackend = createManaged(DEFAULT_BACKEND_LABEL, false);
		}
		return fLocalBackend;
	}

	public static boolean isDeveloper() {
		final String dev = System.getProperty("erlide.devel");
		return dev != null && "true".equals(dev);
	}

	public void addBackendListener(IBackendListener listener) {
		fListeners.add(listener);
	}

	public void removeBackendListener(IBackendListener listener) {
		fListeners.remove(listener);
	}

	private void fireUpdate(IBackend b, int type) {
		new BackendChangeNotifier().notify(b, type);
	}

	/**
	 * Notifies a console listener of additions or removals
	 */
	class BackendChangeNotifier implements ISafeRunnable {

		private IBackendListener fListener;

		private int fType;

		private IBackend fChanged;

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.core.runtime.ISafeRunnable#handleException(java.lang.Throwable)
		 */
		public void handleException(Throwable exception) {
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
		public void notify(IBackend b, int type) {
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
			final Object[] ob = fProjectBackends.values().toArray();
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
				public void run(IBackend b) {
					b.getCodeManager().register(p);
				}
			});
		}
	}

	public void removePlugin(final ICodeBundle p) {
		fPlugins.remove(p);
		getIdeBackend().getCodeManager().unregister(p);
		forEachProjectBackend(new IBackendVisitor() {
			public void run(IBackend b) {
				b.getCodeManager().unregister(p);
			}
		});
	}

	public void forEachProjectBackend(IBackendVisitor visitor) {
		synchronized (fProjectBackendsLock) {
			for (final Object element : fProjectBackends.values()) {
				final IBackend b = (IBackend) element;
				try {
					visitor.run(b);
				} catch (final Exception e) {
				}
			}
		}
	}

	public static String buildNodeName(String label) {
		final String host = getHost();
		return buildNodeLabel(label) + "@" + host;
	}

	public static String buildNodeLabel(String label) {
		if (label.indexOf('_') < 0) {
			return label + "_" + fUniqueId;
		}
		return label;
	}

	// private boolean isExtErlideLabel(String label) {
	// final String[] parts = label.split("_");
	// if (parts.length != 2) {
	// return false;
	// }
	//
	// if (JAVA_NODE_LABEL.equals(parts[0])) {
	// return false;
	// }
	// try {
	// /* final long n = */Long.parseLong(parts[1], 16);
	//
	// synchronized (fProjectBackendsLock) {
	// final IBackend bl = fProjectBackends.get(parts[0]);
	// return bl == null || !bl.getLabel().equals(label);
	// }
	//
	// } catch (final Exception e) {
	// return false;
	// }
	// }

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

	public void resourceChanged(IResourceChangeEvent event) {
		// ErlLogger.debug("+BM: " + event.getType() + " " +
		// event.getResource()
		// + event.getDelta());
		switch (event.getType()) {
		case IResourceChangeEvent.PRE_CLOSE:
		case IResourceChangeEvent.PRE_DELETE:
			break;

		}
	}

	public IBackend getRemoteBackend() {
		return fRemoteBackend;
	}

}
