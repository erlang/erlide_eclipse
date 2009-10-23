/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang;

import java.util.Collection;

import org.eclipse.core.resources.IResource;

import erlang.ErlideScanner;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IErlScanner {

	private final IErlModule module;

	private final String moduleName;

	// private final String moduleFileName;
	// private final String erlidePath;

	public ErlScanner(final IErlModule module, final String initialText,
			final String moduleFileName, final String erlidePath) {
		this.module = module;
		moduleName = createScannerModuleName(module);
		// this.moduleFileName = moduleFileName;
		// this.erlidePath = erlidePath;
		ErlideScanner.initialScan(moduleName, moduleFileName, initialText,
				erlidePath, false);
	}

	public static String createScannerModuleName(final IErlModule module) {
		final IResource res = module.getResource();
		String resName;
		if (res != null) {
			resName = "mod" + res.getFullPath().toPortableString().hashCode()
					+ "_" + res.getName();
		} else {
			// This is not used more than temporarily, so it's OK to have
			// a name that's temporary, as long as it's unique
			resName = "mod" + module.hashCode() + "_";
		}
		return resName;
	}

	public void dispose() {
		ErlideScanner.destroy(moduleName);
	}

	public void replaceText(final int offset, final int removeLength,
			final String newText) {
		ErlideScanner.replaceText(moduleName, offset, removeLength, newText);
	}

	public ErlToken getTokenAt(final int offset) {
		return ErlideScanner.getTokenAt(moduleName, offset);
	}

	public String getScannerModuleName() {
		return moduleName;
	}

	// public void rescan(final String fullText) {
	// ErlideScanner.initialScan(moduleName, moduleFileName, fullText,
	// erlidePath);
	// }

	public Collection<IErlComment> getComments() {
		return module.getComments();
	}
}
