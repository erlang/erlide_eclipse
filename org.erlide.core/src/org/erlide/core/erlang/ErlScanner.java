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

import org.eclipse.core.resources.IResource;
import org.erlide.runtime.backend.BackendManager;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Erlang syntax scanner
 * 
 * @author Eric Merritt
 */
public class ErlScanner implements IErlScanner {

	private final IErlModule fModule;

	private final OtpErlangAtom fMod;

	public ErlScanner(IErlModule module) {
		fModule = module;
		fMod = new OtpErlangAtom(createScannerModuleName());
		create();
	}

	/**
	 * @return
	 */
	public String createScannerModuleName() {
		IResource res = fModule.getResource();
		String resName;
		if (res != null) {
			resName = res.getFullPath().toPortableString();
		} else {
			resName = "res";
		}
		return "_erlide_" + fModule.getErlProject().getElementName() + "_"
				+ resName;
	}

	private void create() {
		try {
			BackendManager.getDefault().getIdeBackend().rpc("erlide_scanner",
					"create", fMod);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public void dispose() {
		try {
			BackendManager.getDefault().getIdeBackend().rpcx("erlide_scanner",
					"destroy", fMod);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	@SuppressWarnings("boxing")
	public ErlToken getTokenAt(int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scanner", "do_getTokenAt", fMod, offset + 1);
		} catch (Exception e) {
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

	public ErlToken[] getTokensAround(int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scanner", "do_getTokensAround", fMod,
					new OtpErlangLong(offset + 1));
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

	@SuppressWarnings("boxing")
	public void insertText(int offset, String text) {
		try {
			BackendManager.getDefault().getIdeBackend().rpc("erlide_scanner",
					"insertText", fMod, offset, text);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	@SuppressWarnings("boxing")
	public void removeText(int offset, int length) {
		try {
			BackendManager.getDefault().getIdeBackend().rpc("erlide_scanner",
					"removeText", fMod, offset, length);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public ErlToken[] getTokens() {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scanner", "do_getTokens", fMod);
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
			r1 = BackendManager.getDefault().getIdeBackend()
					.rpcx("erlide_scanner", "do_getTokenWindow", fMod, offset,
							window);
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
			return new TokenWindow(tt, window);

		}
		return null;
	}

	public OtpErlangAtom getScannerModuleName() {
		return fMod;
	}
}
