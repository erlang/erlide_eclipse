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

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.RGB;
import org.erlide.core.erlang.ErlToken;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.prefs.HighlightStyle;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;
import org.erlide.ui.util.IColorManager;
import org.osgi.service.prefs.Preferences;

import erlang.ErlideScanner;

/**
 * Erlang syntax fScanner
 * 
 * @author Eric Merritt
 */
public class ErlHighlightScanner implements ITokenScanner,
		IPreferenceChangeListener {

	private final Token t_default;
	private final Token t_atom;
	private final Token t_string;
	private final Token t_keyword;
	private final Token t_var;
	private final Token t_char;
	private final Token t_arrow;
	private final Token t_macro;
	private final Token t_integer;
	private final Token t_float;
	private final Token t_comment;

	private final IColorManager fColorManager;
	protected List<ErlToken> fTokens;
	protected int fCrtToken;
	private int rangeLength;
	private int rangeOffset;
	private ISourceViewer fSourceViewer;

	/**
	 * Constructs the rules that define syntax highlighting.
	 * 
	 * @param lmanager
	 *            the color fColorManager
	 * @param editorConfiguration
	 * @param fScanner
	 */
	public ErlHighlightScanner(final IColorManager lmanager,
			ISourceViewer editorConfiguration) {
		fColorManager = lmanager;
		fSourceViewer = editorConfiguration;

		t_string = new Token(getTextAttribute(TokenHighlight.STRING));
		t_keyword = new Token(getTextAttribute(TokenHighlight.KEYWORD));
		t_var = new Token(getTextAttribute(TokenHighlight.VARIABLE));
		t_default = new Token(getTextAttribute(TokenHighlight.DEFAULT));
		t_arrow = new Token(getTextAttribute(TokenHighlight.ARROW));
		t_char = new Token(getTextAttribute(TokenHighlight.CHAR));
		t_macro = new Token(getTextAttribute(TokenHighlight.MACRO));
		t_atom = new Token(getTextAttribute(TokenHighlight.ATOM));
		t_integer = new Token(getTextAttribute(TokenHighlight.INTEGER));
		t_float = new Token(getTextAttribute(TokenHighlight.FLOAT));
		t_comment = new Token(getTextAttribute(TokenHighlight.COMMENT));

	}

	private TextAttribute getTextAttribute(TokenHighlight th) {
		String qualifier = ColoringPreferencePage.COLORS_QUALIFIER
				+ th.getName();
		HighlightStyle data = new HighlightStyle();
		data.load(qualifier, th.getDefaultData());
		new InstanceScope().getNode(qualifier)
				.addPreferenceChangeListener(this);
		return new TextAttribute(fColorManager.getColor(data.getColor()), null,
				data.getStyle());
	}

	public IToken convert(final ErlToken tk) {
		if (tk == ErlToken.EOF) {
			return Token.EOF;
		}

		final String kind = tk.getKind();
		if (kind.equals("string")) {
			return t_string;
		} else if (kind.equals("reserved")) {
			return t_keyword;
		} else if (kind.equals("atom")) {
			return t_atom;
		} else if (kind.equals("var")) {
			return t_var;
		} else if (kind.equals("char")) {
			return t_char;
		} else if (kind.equals("macro")) {
			return t_macro;
		} else if (kind.equals("->")) {
			return t_arrow;
		} else if (kind.equals("integer")) {
			return t_integer;
		} else if (kind.equals("float")) {
			return t_float;
		} else if (kind.equals("comment")) {
			return t_comment;
		} else {
			return t_default; // Token.UNDEFINED;
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
	public void handleColorChange(final String id, final RGB newValue,
			final int style) {
		final Token token = getToken(id);
		fixTokenData(token, newValue, style);
	}

	private Token getToken(final String id) {
		if (TokenHighlight.DEFAULT.getName().equals(id)) {
			return t_default;
		} else if (TokenHighlight.KEYWORD.getName().equals(id)) {
			return t_keyword;
		} else if (TokenHighlight.STRING.getName().equals(id)) {
			return t_string;
		} else if (TokenHighlight.VARIABLE.getName().equals(id)) {
			return t_var;
		} else if (TokenHighlight.COMMENT.getName().equals(id)) {
			return t_comment;
		} else if (TokenHighlight.CHAR.getName().equals(id)) {
			return t_char;
		} else if (TokenHighlight.ATOM.getName().equals(id)) {
			return t_atom;
		} else if (TokenHighlight.ARROW.getName().equals(id)) {
			return t_arrow;
		} else if (TokenHighlight.FLOAT.getName().equals(id)) {
			return t_float;
		} else if (TokenHighlight.INTEGER.getName().equals(id)) {
			return t_integer;
		} else if (TokenHighlight.MACRO.getName().equals(id)) {
			return t_macro;
		}
		return null;
	}

	private void fixTokenData(final Token token, final RGB newValue,
			final int style) {
		final TextAttribute attr = (TextAttribute) token.getData();
		token.setData(new TextAttribute(fColorManager.getColor(newValue), attr
				.getBackground(), style));
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

			// ErlLogger.debug("setRange %s %d:%d (%d:%d)", document,
			// rangeOffset, rangeLength, offset, length);

			final String str = document.get(rangeOffset, rangeLength);
			setText(str);
		} catch (final BadLocationException e) {
			ErlLogger.warn(e);
		}
	}

	private void setText(final String document) {
		if (document == null) {
			return;
		}

		try {
			fCrtToken = -1;

			final String str = document;
			fTokens = ErlideScanner.lightScanString(str, rangeOffset);

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

	public void preferenceChange(PreferenceChangeEvent event) {
		String key = event.getKey();
		Preferences node = event.getNode();
		String newValue = (String) event.getNewValue();
		Token tk = getToken(node.name());
		TextAttribute attr = (TextAttribute) tk.getData();
		if (HighlightStyle.COLOR_KEY.equals(key)) {
			if (newValue == null) {
				// color = dflt.color;
			} else {
				attr = new TextAttribute(fColorManager.getColor(StringConverter
						.asRGB(newValue)), attr.getBackground(), attr
						.getStyle());
			}
		} else if (HighlightStyle.STYLE_KEY.equals(key)) {
			if (newValue == null) {
				// style = dflt.style;
			} else {
				attr = new TextAttribute(attr.getForeground(), attr
						.getBackground(), Integer.parseInt(newValue));
			}
		}
		tk.setData(attr);
		fSourceViewer.invalidateTextPresentation();
	}
}
