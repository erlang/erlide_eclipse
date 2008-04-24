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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;

import erlang.ErlideScanner;

/**
 * Erlang syntax scanner
 * 
 * @author Vlad
 */
public class ErlScanner implements IErlScanner {

	private final IErlModule fModule;

	private final String fMod;

	public ErlScanner(IErlModule module, String initialText) {
		fModule = module;
		fMod = createScannerModuleName(fModule);
		String moduleFileName = "";
		final IResource r = module.getResource();
		if (r instanceof IFile) {
			final IFile f = (IFile) r;
			moduleFileName = f.getLocation().toString();
		}
		ErlideScanner.initialScan(fMod, moduleFileName, initialText);
		// create();
		// insertText(0, initialText);
	}

	/**
	 * @return
	 */

	static public String createScannerModuleName(IErlModule module) {
		final IResource res = module.getResource();
		String resName;
		if (res != null) {
			resName = "mod" + res.getFullPath().toPortableString().hashCode()
					+ "_" + res.getName();
		} else {
			// This is not used more than temporarily, so it's ok to have
			// a name that's temporary, as long as it's unique
			resName = "mod" + module.hashCode() + "_";
		}
		return resName;
	}

	// private void create() {
	// ErlideScanner.create(fMod);
	// }

	public void dispose() {
		ErlideScanner.destroy(fMod);
	}

	// public void insertText(int offset, String text) {
	// // ErlLogger.debug("scanner insert offset=" + offset + " text.length="
	// // + text.length());
	// ErlideScanner.insertText(fMod, offset, text);
	// }

	// public void removeText(int offset, int length) {
	// // ErlLogger.debug("scanner remove offset=" + offset + " length=" +
	// // length);
	// ErlideScanner.removeText(fMod, offset, length);
	// }

	public void replaceText(int offset, int removeLength, String newText) {
		ErlideScanner.replaceText(fMod, offset, removeLength, newText);
	}

	public ErlToken getTokenAt(int offset) {
		return ErlideScanner.getTokenAt(fMod, offset);
	}

	public ErlToken[] getTokensAround(int offset) {
		return ErlideScanner.getTokensAround(fMod, offset);
	}

	public ErlToken[] getTokens() {
		return ErlideScanner.getTokens(fMod);
	}

	public TokenWindow getTokenWindow(int offset, int window) {
		return ErlideScanner.getTokenWindow(fMod, offset, window);
	}

	public String getScannerModuleName() {
		return fMod;
	}
}
