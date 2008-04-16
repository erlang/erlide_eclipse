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
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

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

	@SuppressWarnings("boxing")
	public ErlToken getTokenAt(int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scanner", "do_getTokenAt", "ai", fMod, offset + 1);
		} catch (final Exception e) {
			// e.printStackTrace();
			return null;
		}
		if (r1 == null) {
			return null;
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			final OtpErlangTuple tt = (OtpErlangTuple) t1.elementAt(1);
			return new ErlToken(tt, 0);
		}
		return null;
	}

	@SuppressWarnings("boxing")
	public ErlToken[] getTokensAround(int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scanner", "do_getTokensAround", "ai", fMod,
					offset + 1);
		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}
		if (r1 == null) {
			return null;
		}

		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			final OtpErlangList tt = (OtpErlangList) t1.elementAt(1);

			final ErlToken[] result = new ErlToken[tt.arity()];
			for (int i = 0; i < tt.arity(); i++) {
				result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
			}

		}
		return null;
	}

	// public void modifyText(IDocument doc, DirtyRegion dirtyRegion) {
	// if (doc == null) {
	// return;
	// }
	// if (dirtyRegion == null) {
	// dirtyRegion = new DirtyRegion(0, doc.getLength(),
	// DirtyRegion.INSERT, doc.get());
	// }
	//
	// if (dirtyRegion.getType() == DirtyRegion.REMOVE) {
	// removeText(doc, dirtyRegion.getOffset() + 1, dirtyRegion
	// .getLength());
	// } else if (dirtyRegion.getType() == DirtyRegion.INSERT) {
	// insertText(doc, dirtyRegion.getOffset() + 1, dirtyRegion.getText());
	// }
	// }

	public void insertText(int offset, String text) {
		// ErlLogger.debug("scanner insert offset=" + offset + " text.length="
		// + text.length());
		ErlideScanner.insertText(fMod, offset, text);
	}

	public void removeText(int offset, int length) {
		// ErlLogger.debug("scanner remove offset=" + offset + " length=" +
		// length);
		ErlideScanner.removeText(fMod, offset, length);
	}

	public ErlToken[] getTokens() {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scanner", "do_getTokens", "a", fMod);
		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}
		if (r1 == null) {
			return null;
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			final OtpErlangList tt = (OtpErlangList) t1.elementAt(1);

			final ErlToken[] result = new ErlToken[tt.arity()];
			for (int i = 0; i < tt.arity(); i++) {
				result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
			}
			return result;
		}
		return null;
	}

	@SuppressWarnings("boxing")
	public TokenWindow getTokenWindow(int offset, int window) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scanner", "do_getTokenWindow", "aii", fMod, offset,
					window);
		} catch (final NoBackendException e) {
			ErlLogger.debug(e);
		} catch (final Exception e) {
			ErlLogger.warn(e);
			return null;
		}
		if (r1 == null) {
			return null;
		}

		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			final OtpErlangList tt = (OtpErlangList) t1.elementAt(1);

			final ErlToken[] result = new ErlToken[tt.arity()];
			for (int i = 0; i < tt.arity(); i++) {
				result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
			}
			return new TokenWindow(tt, window);

		}
		return null;
	}

	public String getScannerModuleName() {
		return fMod;
	}
}
