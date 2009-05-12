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
import org.erlide.core.ErlangPlugin;
import org.erlide.core.util.ErlideUtil;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.ICodeBundle;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;

import erlang.ErlangCode;
import erlang.ErlideBackend;

public class CodeManager implements IRegistryChangeListener {

	private final Backend fBackend;

	private final List<PathItem> pathA;
	private final List<PathItem> pathZ;

	private final List<ICodeBundle> codeBundles;

	// only to be called by Backend
	public CodeManager(final Backend b) {
		fBackend = b;
		pathA = new ArrayList<PathItem>(10);
		pathZ = new ArrayList<PathItem>(10);
		codeBundles = new ArrayList<ICodeBundle>(10);
	}

	private PathItem findItem(final List<PathItem> l, final String p) {
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
	private void addPathA(final String path) {
		if (addPath(pathA, path)) {
			ErlangCode.addPathA(fBackend, path);
		}
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#addPathZ(java.lang.String)
	 */
	private void addPathZ(final String path) {
		if (addPath(pathZ, path)) {
			ErlangCode.addPathZ(fBackend, path);
		}
	}

	private boolean addPath(final List<PathItem> l, final String path) {
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
	private void removePathA(final String path) {
		if (removePath(pathA, path)) {
			ErlangCode.removePathA(fBackend, path);
		}
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#removePathZ(java.lang.String)
	 */
	private void removePathZ(final String path) {
		if (removePath(pathZ, path)) {
			ErlangCode.removePathZ(fBackend, path);
		}
	}

	private boolean removePath(final List<PathItem> l, final String path) {
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

	private List<String> getPath(final List<PathItem> l) {
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
	protected boolean loadBeam(final String moduleName, final URL beamPath) {
		final OtpErlangBinary bin = ErlideUtil.getBeamBinary(moduleName,
				beamPath);
		if (bin == null) {
			return false;
		}
		return ErlideBackend.loadBeam(fBackend, moduleName, bin);
	}

	@SuppressWarnings("unchecked")
	private void loadPluginCode(final ICodeBundle p) {

		final Bundle b = p.getBundle();
		ErlLogger.debug("loading plugin " + b.getSymbolicName() + " in "
				+ fBackend.getInfo().getName());

		// TODO Do we have to also check any fragments?
		// see FindSupport.findInFragments

		final IExtensionRegistry reg = RegistryFactory.getRegistry();
		reg.addRegistryChangeListener(this);
		final IConfigurationElement[] els = reg.getConfigurationElementsFor(
				ErlangPlugin.PLUGIN_ID, "codepath");
		for (final IConfigurationElement el : els) {
			final IContributor c = el.getContributor();
			if (c.getName().equals(b.getSymbolicName())) {
				final String dir_path = el.getAttribute("path");
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
							final boolean ok = loadBeam(m, b.getEntry(s));
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

		// load all stub code
		final IConfigurationElement[] stubs = reg.getConfigurationElementsFor(
				ErlangPlugin.PLUGIN_ID, "javaRpcStubs");
		for (final IConfigurationElement stub : stubs) {
			final IContributor c = stub.getContributor();
			if (c.getName().equals(b.getSymbolicName())) {
				final String decl = stub.getAttribute("onlyDeclared");
				// ErlLogger.debug(" STUB: %s %s", stub.getAttribute("class"),
				// decl);
				BackendUtil.generateRpcStub(stub.getAttribute("class"),
						decl == null ? false : Boolean.parseBoolean(decl),
						fBackend);
			}
		}
		// ErlLogger.debug("*done! loading plugin " + b.getSymbolicName());
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#register(ICodeBundle)
	 */
	public void register(final ICodeBundle p) {
		if (codeBundles.indexOf(p) < 0) {
			codeBundles.add(p);
			final String ebinDir = p.getEbinDir();
			if (ebinDir != null) {
				final String localDir = ebinDir.replaceAll("\\\\", "/");
				final boolean accessible = ErlideUtil.isAccessible(fBackend,
						localDir);
				if (accessible) {
					ErlLogger.debug("adding %s to code path for %s", localDir,
							fBackend.getInfo());
					ErlangCode.addPathA(fBackend, localDir);
				} else {
					ErlLogger.debug("loading %s for %s", p.getBundle()
							.getSymbolicName(), fBackend.getInfo());
					loadPluginCode(p);
				}
			} else {
				ErlLogger.warn("Could not find 'ebin' in bundle %s.", p
						.getBundle().getSymbolicName());
				loadPluginCode(p);
			}
			p.start();
		}
	}

	/**
	 * @see org.erlide.runtime.backend.ICodeManager#unregister(ICodeBundle)
	 */
	public void unregister(final ICodeBundle p) {
		final String ebinDir = p.getEbinDir();
		if (ebinDir != null) {
			final String localDir = ebinDir.replaceAll("\\\\", "/");
			ErlLogger.debug("removing %s from code path for %s", localDir,
					fBackend.getInfo());
		}
		codeBundles.remove(p);
		unloadPluginCode(p);
	}

	@SuppressWarnings("unchecked")
	private void unloadPluginCode(final ICodeBundle p) {
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

	private void unloadBeam(final String moduleName) {
		ErlangCode.delete(fBackend, moduleName);
	}

	private static class PathItem {

		public PathItem(final String p) {
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

	public void addPath(final boolean usePathZ, final String path) {
		if (usePathZ) {
			addPathZ(path);
		} else {
			addPathA(path);
		}
	}

	public void removePath(final boolean usePathZ, final String path) {
		if (usePathZ) {
			removePathZ(path);
		} else {
			removePathA(path);
		}
	}

	public void registryChanged(final IRegistryChangeEvent event) {
		ErlLogger.debug("??"
				+ event.getExtensionDeltas()[0].getExtensionPoint()
						.getUniqueIdentifier());
	}

}
