/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.erlide.core.erlang.IErlScanner;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

/**
 * Helper class for match pairs of characters.
 */
public class ErlPairMatcher implements ICharacterPairMatcher {

	protected final IErlScanner fScanner;

	protected int fOffset;

	protected int fStartPos;

	protected int fEndPos;

	protected int fAnchor;

	public ErlPairMatcher(IErlScanner scanner) {
		fScanner = scanner;
	}

	/*
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#match(org.eclipse.jface.text.IDocument,
	 *      int)
	 */
	public IRegion match(IDocument document, int offset) {
		fOffset = offset;
		if (fOffset >= 0 && matchPairsAt() && fStartPos != fEndPos) {
			return new Region(fStartPos, fEndPos - fStartPos + 1);
		}
		return null;
	}

	@SuppressWarnings("boxing")
	private boolean matchPairsAt() {
		if (fScanner == null) {
			return false;
		}
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_pair_match", "match", fOffset,
					new OtpErlangAtom(fScanner.getScannerModuleName()));
			if (r1 instanceof OtpErlangLong) {
				final OtpErlangLong s1 = (OtpErlangLong) r1;
				final int pos = s1.intValue() - 1, offset = fOffset - 1;
				if (pos < offset) {
					fStartPos = pos;
					fEndPos = offset;
					fAnchor = RIGHT;
				} else {
					fStartPos = offset;
					fEndPos = pos;
					fAnchor = LEFT;
				}
				return true;
			}
		} catch (ErlangRpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return false;
	}

	/*
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#getAnchor()
	 */
	public int getAnchor() {
		return fAnchor;
	}

	/*
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#dispose()
	 */
	public void dispose() {
	}

	/*
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#clear()
	 */
	public void clear() {
	}
}
