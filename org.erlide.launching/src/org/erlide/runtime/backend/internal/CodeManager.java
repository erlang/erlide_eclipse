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
package org.erlide.runtime.backend.internal;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IRegistryChangeEvent;
import org.eclipse.core.runtime.IRegistryChangeListener;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.InterfacePlugin;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.BuildBackend;
import org.erlide.runtime.backend.ExecutionBackend;
import org.erlide.runtime.backend.ICodeManager;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;

import erlang.ErlangCode;
import erlang.ErlideBackend;

public class CodeManager implements ICodeManager, IRegistryChangeListener {

	private final ExecutionBackend fBackend;

	private final List<PathItem> pathA;
	private final List<PathItem> pathZ;

	private final List<ICodeBundle> plugins;

	// only to be called by AbstractBackend
	CodeManager(ExecutionBackend b) {
		fBackend = b;
		pathA = new ArrayList<PathItem>(10);
		pathZ = new ArrayList<PathItem>(10);
		plugins = new ArrayList<ICodeBundle>(10);
	}

	private PathItem findItem(List<PathItem> l, String p) {
		final Iterator<PathItem> i = l.iterator();
		while (i.hasNext()) {
			final PathItem it = i.next();
			if (it.path.equals(p)) {
				return it;
			}
		}
		return null;
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#addPathA(java.lang.String)
	 */
	private void addPathA(String path) {
		if (addPath(pathA, path)) {
			ErlangCode.addPathA(fBackend, path);
		}
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#addPathZ(java.lang.String)
	 */
	private void addPathZ(String path) {
		if (addPath(pathZ, path)) {
			ErlangCode.addPathZ(fBackend, path);
		}
	}

	private boolean addPath(List<PathItem> l, String path) {
		if (path == null) {
			return false;
		}

		final PathItem it = findItem(l, path);
		if (it == null) {
			l.add(new PathItem(path));
			return true;
		}
		it.incRef();
		return false;
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#removePathA(java.lang.String)
	 */
	private void removePathA(String path) {
		if (removePath(pathA, path)) {
			ErlangCode.removePathA(fBackend, path);
		}
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#removePathZ(java.lang.String)
	 */
	private void removePathZ(String path) {
		if (removePath(pathZ, path)) {
			ErlangCode.removePathZ(fBackend, path);
		}
	}

	private boolean removePath(List<PathItem> l, String path) {
		if (path == null) {
			return false;
		}
		final PathItem it = findItem(l, path);
		if (it != null) {
			it.decRef();
			if (it.ref <= 0) {
				l.remove(it);
				return true;
			}
		}
		return false;
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#getPathA()
	 */
	public List<String> getPathA() {
		return getPath(pathA);
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#getPathZ()
	 */
	public List<String> getPathZ() {
		return getPath(pathZ);
	}

	private List<String> getPath(List<PathItem> l) {
		final List<String> r = new ArrayList<String>(l.size());
		for (int i = 0; i < l.size(); i++) {
			final String p = l.get(i).path;
			if (p != null) {
				r.add(p);
			}
		}
		return r;
	}

	/**
	 * @param moduleName
	 * @param beamPath
	 * @return boolean
	 */
	protected boolean loadBeam(String moduleName, URL beamPath) {
		final OtpErlangBinary bin = getBeam(moduleName, beamPath, 2048);
		if (bin == null) {
			return false;
		}
		return ErlideBackend.loadBeam(fBackend, moduleName, bin);
	}

	/**
	 * Method getBeam
	 * 
	 * @param moduleName
	 *            String
	 * @param beamPath
	 *            String
	 * @param bufSize
	 *            int
	 * @return OtpErlangBinary
	 */
	private OtpErlangBinary getBeam(String moduleName, URL beamPath, int bufSize) {
		try {
			byte[] b = new byte[bufSize];
			final BufferedInputStream s = new BufferedInputStream(beamPath
					.openStream());
			try {
				final int r = s.read(b);

				if (r == b.length) {
					b = null;
					return getBeam(moduleName, beamPath, bufSize * 2);
				} else if (r > 0) {
					byte[] bm = new byte[r];
					System.arraycopy(b, 0, bm, 0, r);
					b = null;
					return new OtpErlangBinary(bm);
				} else {
					return null;
				}
			} finally {
				s.close();
			}
		} catch (final IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	// TODO move this in an util class!
	public static byte[] concat(byte[][] arrs) {
		int total = 0;
		for (byte[] arr : arrs) {
			total += arr.length;
		}
		byte[] result = new byte[total];
		int crt = 0;
		for (byte[] arr : arrs) {
			System.arraycopy(arr, 0, result, crt, arr.length);
			crt += arr.length;
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private void loadPluginCode(ICodeBundle p) {
		if (fBackend.getInfo() != null && !fBackend.getInfo().isErlide()) {
			return;
		}

		final Bundle b = p.getBundle();
		ErlLogger.debug("loading plugin " + b.getSymbolicName());

		// TODO Do we have to also check any fragments?
		// see FindSupport.findInFragments

		IExtensionRegistry reg = RegistryFactory.getRegistry();
		reg.addRegistryChangeListener(this);
		IConfigurationElement[] els = reg.getConfigurationElementsFor(
				InterfacePlugin.PLUGIN_ID, "codepath");
		for (IConfigurationElement el : els) {
			IContributor c = el.getContributor();
			if (c.getName().equals(b.getSymbolicName())) {
				String dir_path = el.getAttribute("path");

				ErlLogger.debug("    " + dir_path);

				final String ver = fBackend.getCurrentVersion();
				Enumeration e = b.getEntryPaths(dir_path + "/" + ver);
				if (e == null || !e.hasMoreElements()) {
					e = b.getEntryPaths(dir_path);
				}
				if (e == null) {
					ErlLogger.debug("* !!! error loading plugin "
							+ b.getSymbolicName());
					return;
				}
				while (e.hasMoreElements()) {
					final String s = (String) e.nextElement();
					final Path path = new Path(s);
					if (path.getFileExtension() != null
							&& "beam".compareTo(path.getFileExtension()) == 0) {
						final String m = path.removeFileExtension()
								.lastSegment();
						// ErlLogger.debug(" " + m);
						try {
							boolean ok = loadBeam(m, b.getEntry(s));
							if (!ok) {
								ErlLogger.error("Could not load %s", m);
							}
						} catch (final Exception ex) {
							ex.printStackTrace();
						}
					}
				}
			}
		}

		// load all stub code
		IConfigurationElement[] stubs = reg.getConfigurationElementsFor(
				ErlangLaunchPlugin.PLUGIN_ID, "javaRpcStubs");
		for (IConfigurationElement stub : stubs) {
			IContributor c = stub.getContributor();
			if (c.getName().equals(b.getSymbolicName())) {
				String decl = stub.getAttribute("onlyDeclared");
				ErlLogger.debug("  STUB: %s %s", stub.getAttribute("class"),
						decl);
				BackendUtil.generateRpcStub(stub.getAttribute("class"),
						(decl == null) ? false : Boolean.parseBoolean(decl),
						(BuildBackend) fBackend);
			}
		}
		ErlLogger.debug("*done! loading plugin " + b.getSymbolicName());
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#register(ICodeBundle)
	 */
	public void register(ICodeBundle p) {
		if (plugins.indexOf(p) < 0) {
			plugins.add(p);
			loadPluginCode(p);
			p.start();
		}
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#unregister(ICodeBundle)
	 */
	public void unregister(ICodeBundle p) {
		plugins.remove(p);
		unloadPluginCode(p);
	}

	@SuppressWarnings("unchecked")
	private void unloadPluginCode(ICodeBundle p) {
		if (fBackend.getInfo() != null && !fBackend.getInfo().isErlide()) {
			return;
		}

		// TODO Do we have to also check any fragments?
		// see FindSupport.findInFragments

		final Bundle b = p.getBundle();
		final String ver = fBackend.getCurrentVersion();
		Enumeration e = b.getEntryPaths("/ebin/" + ver);
		if (e == null) {
			return;
		}
		ErlLogger.debug("*> really unloading plugin " + p.getClass().getName());
		if (!e.hasMoreElements()) {
			e = b.getEntryPaths("/ebin");
		}
		while (e.hasMoreElements()) {
			final String s = (String) e.nextElement();
			final Path path = new Path(s);
			if (path.getFileExtension() != null
					&& "beam".compareTo(path.getFileExtension()) == 0) {
				final String m = path.removeFileExtension().lastSegment();
				unloadBeam(m);
			}
		}
	}

	private void unloadBeam(String moduleName) {
		ErlangCode.delete(fBackend, moduleName);
	}

	private static class PathItem {

		public PathItem(String p) {
			path = p;
			ref = 1;
		}

		public String path;

		public int ref;

		public void incRef() {
			ref++;
		}

		public void decRef() {
			ref--;
		}
	}

	public void addPath(boolean usePathZ, String path) {
		if (usePathZ) {
			addPathZ(path);
		} else {
			addPathA(path);
		}
	}

	public void removePath(boolean usePathZ, String path) {
		if (usePathZ) {
			removePathZ(path);
		} else {
			removePathA(path);
		}
	}

	public void registryChanged(IRegistryChangeEvent event) {
		ErlLogger.debug("??"
				+ event.getExtensionDeltas()[0].getExtensionPoint()
						.getUniqueIdentifier());
	}
}
