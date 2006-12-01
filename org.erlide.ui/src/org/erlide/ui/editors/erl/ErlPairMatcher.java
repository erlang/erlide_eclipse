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

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlScanner;

/**
 * Helper class for match pairs of characters.
 */
public class ErlPairMatcher implements ICharacterPairMatcher {

	protected IDocument fDocument;

	protected IErlScanner fScanner;

	protected int fOffset;

	protected int fStartPos;

	protected int fEndPos;

	protected int fAnchor;

	static final String[] BRACKETS = { "{", "}", "(", ")", "[", "]", "<", ">",
			"<<", ">>" };

	public ErlPairMatcher(IErlScanner scanner) {
		fScanner = scanner;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#match(org.eclipse.jface.text.IDocument,
	 *      int)
	 */
	public IRegion match(IDocument document, int offset) {
		fOffset = offset;

		if (fOffset < 0) {
			return null;
		}

		fDocument = document;

		if (fDocument != null && matchPairsAt() && fStartPos != fEndPos) {
			return new Region(fStartPos, fEndPos - fStartPos + 1);
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#getAnchor()
	 */
	public int getAnchor() {
		return fAnchor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#dispose()
	 */
	public void dispose() {
		clear();
		fDocument = null;
	}

	/*
	 * @see org.eclipse.jface.text.source.ICharacterPairMatcher#clear()
	 */
	public void clear() {
		fScanner = null;
	}

	protected boolean matchPairsAt() {
		if (fScanner == null) {
			return false;
		}

		int i;
		int pairIndex1 = BRACKETS.length;
		int pairIndex2 = BRACKETS.length;

		fStartPos = -1;
		fEndPos = -1;

		// get the char preceding the start position
		try {
			final ErlToken prevToken = fScanner.getTokenAt(fOffset - 1);
			if (prevToken != null) {

				// search for opening peer character next to the activation
				// point
				for (i = 0; i < BRACKETS.length; i = i + 2) {
					if (prevToken.getKind().equals(BRACKETS[i])) {
						fStartPos = fOffset - 1;
						pairIndex1 = i;
					}
				}

				// search for closing peer character next to the activation
				// point
				for (i = 1; i < BRACKETS.length; i = i + 2) {
					if (prevToken.getKind().equals(BRACKETS[i])) {
						fEndPos = fOffset - 1;
						pairIndex2 = i;
					}
				}

				if (fEndPos > -1) {
					fAnchor = RIGHT;
					fStartPos = searchForOpeningPeer(fEndPos,
							BRACKETS[pairIndex2 - 1], BRACKETS[pairIndex2],
							fDocument);
					if (fStartPos > -1) {
						return true;
					} else {
						fEndPos = -1;
					}
				} else if (fStartPos > -1) {
					fAnchor = LEFT;
					fEndPos = searchForClosingPeer(fStartPos,
							BRACKETS[pairIndex1], BRACKETS[pairIndex1 + 1],
							fDocument);
					if (fEndPos > -1) {
						return true;
					} else {
						fStartPos = -1;
					}
				}
			}

		} catch (final BadLocationException x) {
		}

		return false;
	}

	protected int searchForClosingPeer(int offset, String openingPeer,
			String closingPeer, IDocument document) throws BadLocationException {
		if (fScanner == null) {
			return -1;
		}

		final ErlToken[] tks = fScanner.getTokens();
		if (tks == null) {
			return -1;
		}

		for (int i = 0; i < tks.length; i++) {
			if (tks[i].getOffset() + tks[i].getLength() >= offset) {
				int count = 0;
				for (int j = i; j < tks.length - 1; j++) {
					if (tks[j].getKind().equals(openingPeer)) {
						count++;
					} else if (tks[j].getKind().equals(closingPeer)) {
						count--;
						if (count == 0) {
							return tks[j].getOffset() - 1;
						}
					}
				}
			}
		}
		return -1;
	}

	protected int searchForOpeningPeer(int offset, String openingPeer,
			String closingPeer, IDocument document) throws BadLocationException {
		if (fScanner == null) {
			return -1;
		}

		final ErlToken[] tks = fScanner.getTokens();
		if (tks == null) {
			return -1;
		}

		for (int i = 0; i < tks.length; i++) {
			if (tks[i].getOffset() + tks[i].getLength() >= offset) {
				int count = 0;
				for (int j = i; j >= 0; j--) {
					if (tks[j].getKind().equals(openingPeer)) {
						count--;
						if (count == 0) {
							return tks[j].getOffset() - 1;
						}
					} else if (tks[j].getKind().equals(closingPeer)) {
						count++;
					}
				}
			}
		}
		return -1;
	}

}
