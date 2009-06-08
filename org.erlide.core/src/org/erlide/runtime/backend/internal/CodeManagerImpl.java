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
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.backend.ErlangCode;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.CodeManager;
import org.erlide.runtime.backend.ErlideBackend;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangBinary;

public class CodeManagerImpl implements CodeManager {

	private final Backend fBackend;

	private final List<PathItem> pathA;
	private final List<PathItem> pathZ;

	private final List<CodeBundle> registeredBundles;

	// only to be called by ErlideBackend
	public CodeManagerImpl(final ErlideBackend b) {
		fBackend = b;
		pathA = new ArrayList<PathItem>(10);
		pathZ = new ArrayList<PathItem>(10);
		registeredBundles = new ArrayList<CodeBundle>(10);
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
	 * @see org.erlide.runtime.backend.CodeManager#addPathA(java.lang.String)
	 */
	private void addPathA(final String path) {
		if (addPath(pathA, path)) {
			ErlangCode.addPathA(fBackend, path);
		}
	}

	/**
	 * @see org.erlide.runtime.backend.CodeManager#addPathZ(java.lang.String)
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
	 * @see org.erlide.runtime.backend.CodeManager#removePathA(java.lang.String)
	 */
	private void removePathA(final String path) {
		if (removePath(pathA, path)) {
			ErlangCode.removePathA(fBackend, path);
		}
	}

	/**
	 * @see org.erlide.runtime.backend.CodeManager#removePathZ(java.lang.String)
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
		return ErlBackend.loadBeam(fBackend, moduleName, bin);
	}

	@SuppressWarnings("unchecked")
	private void loadPluginCode(final CodeBundle p) {

		final Bundle b = p.getBundle();
		ErlLogger.debug("loading plugin " + b.getSymbolicName() + " in "
				+ fBackend.getInfo().getName());

		// TODO Do we have to also check any fragments?
		// see FindSupport.findInFragments

		final IExtensionRegistry reg = RegistryFactory.getRegistry();
		// reg.addRegistryChangeListener(this);
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
				ErlBackend.generateRpcStub(stub.getAttribute("class"),
						decl == null ? false : Boolean.parseBoolean(decl),
						fBackend);
			}
		}
		// ErlLogger.debug("*done! loading plugin " + b.getSymbolicName());
	}

	/**
	 * @see org.erlide.runtime.backend.CodeManager#addPlugin(CodeBundle)
	 */
	public void register(final Bundle b) {
		CodeBundle p = new CodeBundle(b);
		if (registeredBundles.indexOf(p) < 0) {
			registeredBundles.add(p);
			registerBundle(p);
		}
	}

	private void registerBundle(final CodeBundle p) {
		final Collection<String> ebinDirs = p.getEbinDirs();
		if (ebinDirs != null) {
			for (String ebinDir : ebinDirs) {
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
			}
		} else {
			ErlLogger.warn("Could not find 'ebin' in bundle %s.", p.getBundle()
					.getSymbolicName());
			loadPluginCode(p);
		}
	}

	/**
	 * @see org.erlide.runtime.backend.CodeManager#unregister(CodeBundle)
	 */
	public void unregister(final Bundle b) {
		CodeBundle p = findBundle(b);
		if (p == null) {
			return;
		}
		registeredBundles.remove(p);
		unloadPluginCode(p);
	}

	private CodeBundle findBundle(Bundle b) {
		for (CodeBundle p : registeredBundles) {
			if (p.getBundle() == b) {
				return p;
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	private void unloadPluginCode(final CodeBundle p) {
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

	public void registerBundles() {
		for (CodeBundle p : registeredBundles) {
			registerBundle(p);
		}
	}

}
