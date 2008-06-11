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
package org.erlide.ui.editors.erl;

import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.erlide.core.erlang.ErlToken;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.util.IColorManager;

import erlang.ErlideScanner2;

/**
 * Erlang syntax fScanner
 * 
 * @author Eric Merritt
 */
public class ErlHighlightScanner implements ITokenScanner {

	public enum MyErlToken {
		t_def(PreferenceConstants.DEFAULT), t_atom(PreferenceConstants.ATOM), t_attribute(
				PreferenceConstants.ATTRIBUTE), t_string(
				PreferenceConstants.STRING), t_keyword(
				PreferenceConstants.KEYWORD), t_var(
				PreferenceConstants.VARIABLE), t_char(PreferenceConstants.CHAR), t_arrow(
				PreferenceConstants.ARROW), t_guard(PreferenceConstants.GUARD), t_bif(
				PreferenceConstants.BIF), t_macro(PreferenceConstants.MACRO), t_record(
				PreferenceConstants.RECORD), t_integer(
				PreferenceConstants.INTEGER), t_float(PreferenceConstants.FLOAT), t_comment(
				PreferenceConstants.COMMENT), ;

		private String pref;

		MyErlToken(final String apref) {
			pref = apref;
		}
	};

	private final Token t_def;
	private final Token t_atom;
	private final Token t_attribute;
	private final Token t_string;
	private final Token t_keyword;
	private final Token t_var;
	private final Token t_char;
	private final Token t_arrow;
	private final Token t_guard;
	private final Token t_bif;
	private final Token t_macro;
	private final Token t_record;
	private final Token t_integer;
	private final Token t_float;
	private final Token t_comment;

	private final IColorManager fColorManager;
	protected List<ErlToken> fTokens;
	protected int fCrtToken;
	private int rangeLength;
	private int rangeOffset;

	/**
	 * Constructs the rules that define syntax highlighting.
	 * 
	 * @param lmanager
	 *            the color fColorManager
	 * @param fScanner
	 */
	public ErlHighlightScanner(final IColorManager lmanager) {
		fColorManager = lmanager;
		final IPreferenceStore store = ErlideUIPlugin.getDefault()
				.getPreferenceStore();

		t_attribute = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.ATTRIBUTE)), null, SWT.NORMAL));

		t_string = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.STRING)), null, SWT.NORMAL));

		t_keyword = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.KEYWORD)), null, SWT.BOLD));

		t_var = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.VARIABLE)), null, SWT.NORMAL));

		t_def = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.DEFAULT)), null, SWT.NORMAL));

		t_arrow = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.ARROW)), null, SWT.BOLD));

		t_char = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.CHAR)), null, SWT.NORMAL));

		t_bif = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.BIF)), null, SWT.BOLD));

		t_guard = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.GUARD)), null, SWT.NORMAL));

		t_macro = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.MACRO)), null, SWT.NORMAL));

		t_record = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.RECORD)), null, SWT.NORMAL));

		t_atom = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.ATOM)), null, SWT.NORMAL));

		t_integer = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.INTEGER)), null, SWT.NORMAL));

		t_float = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.FLOAT)), null, SWT.NORMAL));

		t_comment = new Token(new TextAttribute(fColorManager
				.getColor(PreferenceConverter.getColor(store,
						PreferenceConstants.COMMENT)), null, SWT.NORMAL));
	}

	public IToken convert(final ErlToken tk) {
		if (tk == ErlToken.EOF) {
			return Token.EOF;
		}

		final String kind = tk.getKind();
		if (kind.equals("attribute")) {
			return t_attribute;
		} else if (kind.equals("string")) {
			return t_string;
		} else if (kind.equals("reserved")) {
			return t_keyword;
		} else if (kind.equals("atom")) {
			return t_atom;
		} else if (kind.equals("record")) {
			return t_record;
		} else if (kind.equals("var")) {
			return t_var;
		} else if (kind.equals("char")) {
			return t_char;
		} else if (kind.equals("macro")) {
			return t_macro;
		} else if (kind.equals("->")) {
			return t_arrow;
		} else if (kind.equals("bif")) {
			return t_bif;
		} else if (kind.equals("guard_bif")) {
			return t_guard;
		} else if (kind.equals("integer")) {
			return t_integer;
		} else if (kind.equals("float")) {
			return t_float;
		} else if (kind.equals("comment")) {
			return t_comment;
		} else {
			return t_def; // Token.UNDEFINED;
		}
	}

	/**
	 * Handle a color change
	 * 
	 * @param id
	 *            the color id
	 * @param newValue
	 *            the new value of the color
	 */
	public void handleColorChange(final String id, final RGB newValue) {
		final Token token = getToken(id);
		fixTokenData(token, newValue);
	}

	private Token getToken(final String id) {
		if (PreferenceConstants.ATTRIBUTE.equals(id)) {
			return t_attribute;
		} else if (PreferenceConstants.DEFAULT.equals(id)) {
			return t_def;
		} else if (PreferenceConstants.KEYWORD.equals(id)) {
			return t_keyword;
		} else if (PreferenceConstants.STRING.equals(id)) {
			return t_string;
		} else if (PreferenceConstants.VARIABLE.equals(id)) {
			return t_var;
		} else if (PreferenceConstants.COMMENT.equals(id)) {
			return t_comment;
		} else if (PreferenceConstants.CHAR.equals(id)) {
			return t_char;
		} else if (PreferenceConstants.ATOM.equals(id)) {
			return t_atom;
		} else if (PreferenceConstants.ARROW.equals(id)) {
			return t_arrow;
		} else if (PreferenceConstants.RECORD.equals(id)) {
			return t_record;
		} else if (PreferenceConstants.FLOAT.equals(id)) {
			return t_float;
		} else if (PreferenceConstants.BIF.equals(id)) {
			return t_bif;
		} else if (PreferenceConstants.INTEGER.equals(id)) {
			return t_integer;
		} else if (PreferenceConstants.GUARD.equals(id)) {
			return t_guard;
		} else if (PreferenceConstants.MACRO.equals(id)) {
			return t_macro;
		}
		return null;
	}

	private void fixTokenData(final Token token, final RGB newValue) {
		final TextAttribute attr = (TextAttribute) token.getData();
		token.setData(new TextAttribute(fColorManager.getColor(newValue), attr
				.getBackground(), attr.getStyle()));
	}

	public void setRange(final IDocument document, final int offset,
			final int length) {
		if (document == null) {
			return;
		}
		try {
			final int line1 = document.getLineOfOffset(offset);
			final int line2 = document.getLineOfOffset(offset + length);
			rangeOffset = document.getLineOffset(line1);
			rangeLength = document.getLineOffset(line2) - rangeOffset
					+ document.getLineLength(line2);

			// ErlLogger.debug("§§§ setRange %s %d:%d (%d:%d)", document,
			// rangeOffset, rangeLength, offset, length);

			final String str = document.get(rangeOffset, rangeLength);
			setText(str);
		} catch (final BadLocationException e) {
			e.printStackTrace();
		}
	}

	private void setText(final String document) {
		if (document == null) {
			return;
		}

		try {
			fCrtToken = -1;

			final String str = document;
			fTokens = ErlideScanner2.lightScanString(str, rangeOffset);

		} catch (final ErlangRpcException e) {
			// e.printStackTrace();
		} catch (final BackendException e) {
			// e.printStackTrace();
		}
	}

	public IToken nextToken() {
		return convert(nextErlToken());
	}

	public int getTokenOffset() {
		if (fTokens == null) {
			return 0;
		}

		if (fCrtToken >= fTokens.size()) {
			return 0;
		}

		final ErlToken tk = fTokens.get(fCrtToken);
		return tk.getOffset();
	}

	public int getTokenLength() {
		if (fTokens == null) {
			return 0;
		}

		if (fCrtToken >= fTokens.size()) {
			return 0;
		}

		final ErlToken tk = fTokens.get(fCrtToken);
		return tk.getLength();
	}

	public ErlToken nextErlToken() {
		if (fTokens == null) {
			return ErlToken.EOF;
		}

		fCrtToken++;
		if (fCrtToken >= fTokens.size()) {
			return ErlToken.EOF;
		}

		final ErlToken tk = fTokens.get(fCrtToken);
		if (tk.getKind() == null) {
			return ErlToken.EOF;
		}
		return fTokens.get(fCrtToken);
	}

}
