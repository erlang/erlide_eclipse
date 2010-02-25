/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.BackendShell;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.console.IoRequest.IoRequestKind;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.internal.CodeManager;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public final class ErlideBackend extends Backend implements IDisposable,
		IStreamListener, ILaunchesListener2 {

	private final CodeManager codeManager;
	private IStreamsProxy proxy;
	private BackendShellManager shellManager;

	private ILaunch launch;

	public ErlideBackend(final RuntimeInfo info) throws BackendException {
		super(info);
		codeManager = new CodeManager(this);
	}

	@Override
	public void dispose() {
		try {
			if (launch != null) {
				launch.terminate();
			}
		} catch (DebugException e) {
			e.printStackTrace();
		}
		shellManager.dispose();
		dispose(false);
	}

	@Override
	public void initializeRuntime() {
		super.initializeRuntime();
		shellManager = new BackendShellManager();
	}

	@Override
	public void dispose(final boolean restart) {
		ErlLogger.debug("disposing backend " + getName());
		super.dispose(restart);
	}

	@Override
	public synchronized void restart() {
		super.restart();
		codeManager.reRegisterBundles();
		// initErlang();
		// fixme eventdaemon
	}

	public void removePath(final String path) {
		codeManager.removePath(path);
	}

	public void addPath(final boolean usePathZ, final String path) {
		codeManager.addPath(usePathZ, path);
	}

	@Override
	public void initErlang(boolean monitor, boolean watch) {
		super.initErlang(monitor, watch);
		ErlangCore.getBackendManager().addBackendListener(getEventDaemon());
	}

	public void register(CodeBundle bundle) {
		codeManager.register(bundle);
	}

	public void unregister(Bundle b) {
		codeManager.unregister(b);
	}

	public void setTrapExit(boolean contains) {
	}

	public void streamAppended(String text, IStreamMonitor monitor) {
		if (monitor == proxy.getOutputStreamMonitor()) {
			// System.out.println(getName() + " OUT " + text);
		} else if (monitor == proxy.getErrorStreamMonitor()) {
			// System.out.println(getName() + " ERR " + text);
		} else {
			// System.out.println("???" + text);
		}
	}

	public ILaunch getLaunch() {
		return launch;
	}

	public void setLaunch(ILaunch launch2) {
		this.launch = launch2;
	}

	public BackendShell getShell(String id) {
		final BackendShell shell = shellManager.openShell(id);
		if (proxy != null) {
			IStreamMonitor errorStreamMonitor = proxy.getErrorStreamMonitor();
			errorStreamMonitor.addListener(new IStreamListener() {
				public void streamAppended(String text, IStreamMonitor monitor) {
					shell.add(text, IoRequestKind.STDERR);
				}
			});
			IStreamMonitor outputStreamMonitor = proxy.getOutputStreamMonitor();
			outputStreamMonitor.addListener(new IStreamListener() {
				public void streamAppended(String text, IStreamMonitor monitor) {
					shell.add(text, IoRequestKind.STDOUT);
				}
			});
		}
		return shell;
	}

	@Override
	public boolean isDistributed() {
		return !getInfo().getNodeName().equals("");
	}

	@Override
	public void input(String s) throws IOException {
		if (!isStopped()) {
			proxy.write(s);
		}
	}

	public void launchesTerminated(ILaunch[] launches) {
		for (ILaunch aLaunch : launches) {
			if (aLaunch == launch) {
				stop();
			}
		}
	}

	public void launchesAdded(ILaunch[] launches) {
	}

	public void launchesChanged(ILaunch[] launches) {
	}

	public void launchesRemoved(ILaunch[] launches) {
	}

	public void setStreamsProxy(IStreamsProxy streamsProxy) {
		proxy = streamsProxy;
		if (proxy != null) {
			IStreamMonitor errorStreamMonitor = proxy.getErrorStreamMonitor();
			errorStreamMonitor.addListener(this);
			IStreamMonitor outputStreamMonitor = proxy.getOutputStreamMonitor();
			outputStreamMonitor.addListener(this);
		}
	}

	private class BackendShellManager implements IDisposable {

		private final HashMap<String, BackendShell> fShells;

		public BackendShellManager() {
			fShells = new HashMap<String, BackendShell>();
		}

		public BackendShell getShell(final String id) {
			final BackendShell shell = fShells.get(id);
			return shell;
		}

		public synchronized BackendShell openShell(final String id) {
			BackendShell shell = getShell(id);
			if (shell == null) {
				shell = new BackendShell(ErlideBackend.this, id);
				fShells.put(id, shell);
			}
			return shell;
		}

		public synchronized void closeShell(final String id) {
			final BackendShell shell = getShell(id);
			if (shell != null) {
				fShells.remove(id);
				shell.close();
			}
		}

		public void dispose() {
			final Collection<BackendShell> c = fShells.values();
			for (final BackendShell backendShell : c) {
				backendShell.close();
			}
			fShells.clear();
		}
	}

	public void addProjectPath(IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final String outDir = project.getLocation()
				.append(prefs.getOutputDir()).toOSString();
		if (outDir.length() > 0) {
			ErlLogger.debug("backend %s: add path %s", getName(), outDir);
			if (isDistributed()) {
				boolean accessible = ErlideUtil.isAccessible(this, outDir);
				if (accessible) {
					addPath(false/* prefs.getUsePathZ() */, outDir);
				} else {
					loadBeamsFromDir(outDir);
				}
			} else {
				final File f = new File(outDir);
				for (final File file : f.listFiles()) {
					String name = file.getName();
					name = name.substring(0, name.length() - 5);
					try {
						ErlideUtil.loadModuleViaInput(this, project, name);
					} catch (final ErlModelException e) {
						e.printStackTrace();
					} catch (final IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}

	private void loadBeamsFromDir(final String outDir) {
		File dir = new File(outDir);
		for (File f : dir.listFiles()) {
			final Path path = new Path(f.getPath());
			if (path.getFileExtension() != null
					&& "beam".compareTo(path.getFileExtension()) == 0) {
				final String m = path.removeFileExtension().lastSegment();
				try {
					boolean ok = false;
					final OtpErlangBinary bin = ErlideUtil.getBeamBinary(m,
							path);
					if (bin != null) {
						ok = ErlBackend.loadBeam(this, m, bin);
					}
					if (!ok) {
						ErlLogger.error("Could not load %s", m);
					}
				} catch (final Exception ex) {
					ErlLogger.warn(ex);
				}
			}
		}
	}
}
