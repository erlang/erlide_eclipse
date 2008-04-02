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

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;

import erlang.ErlidePairMatch;

/**
 * Helper class for match pairs of characters.
 */
public class ErlPairMatcher implements ICharacterPairMatcher {

	protected final String scannerModuleName;

	protected int fOffset;

	protected int fStartPos;

	protected int fEndPos;

	protected int fAnchor;

	private ErlPairMatcher(String scannerModuleName) {
		this.scannerModuleName = scannerModuleName;
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

	private boolean matchPairsAt() {
		if (scannerModuleName == null) {
			return false;
		}
		OtpErlangObject r1 = null;
		try {
			r1 = ErlidePairMatch.match(fOffset, scannerModuleName);
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
		} catch (final Exception e) {
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
