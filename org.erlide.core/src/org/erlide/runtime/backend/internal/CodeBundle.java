/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.util.ErlideUtil;
import org.osgi.framework.Bundle;

public class CodeBundle {

	private final Bundle bundle;

	public CodeBundle(Bundle b) {
		this.bundle = b;
	}

	public Bundle getBundle() {
		return bundle;
	}

	public Collection<String> getEbinDirs() {
		List<String> result = new ArrayList<String>();
		for (String path : getCodePathExtensions()) {
			result.add(ErlideUtil.getPath(path, bundle));
		}
		return result;
	}

	private Collection<String> getCodePathExtensions() {
		List<String> result = new ArrayList<String>();

		// TODO Do we have to also check any fragments?
		// see FindSupport.findInFragments

		final IExtensionRegistry reg = RegistryFactory.getRegistry();
		// reg.addRegistryChangeListener(this);
		final IConfigurationElement[] els = reg.getConfigurationElementsFor(
				ErlangPlugin.PLUGIN_ID, "codepath");
		for (final IConfigurationElement el : els) {
			final IContributor c = el.getContributor();
			if ("beam_dir".equals(el.getName())
					&& c.getName().equals(bundle.getSymbolicName())) {
				final String path = el.getAttribute("path");
				final String context = el.getAttribute("context");
				// TODO handle context!
				if (!result.contains(path)) {
					result.add(path);
				}
			}
		}
		return result;
	}

	public Collection<String> getPluginCode() {
		List<String> result = new ArrayList<String>();
		Collection<String> dirs = getCodePathExtensions();
		for (String dir : dirs) {
			final Path path = new Path(dir);
			if (path.getFileExtension() != null
					&& "beam".compareTo(path.getFileExtension()) == 0) {
				final String m = path.removeFileExtension().lastSegment();
				result.add(m);
			}
		}
		return result;
	}

}
