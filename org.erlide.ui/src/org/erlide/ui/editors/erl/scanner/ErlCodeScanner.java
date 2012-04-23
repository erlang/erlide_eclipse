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
package org.erlide.ui.editors.erl.scanner;

import java.util.List;

import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.erlide.backend.BackendException;
import org.erlide.core.internal.model.erlang.ErlideScanner;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.prefs.HighlightStyle;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.prefs.plugin.ColoringPreferencePage;
import org.erlide.ui.util.IColorManager;
import org.osgi.service.prefs.Preferences;

public class ErlCodeScanner implements ITokenScanner, IPreferenceChangeListener {

    private static Token t_default;
    private static Token t_atom;
    private static Token t_string;
    private static Token t_keyword;
    private static Token t_var;
    private static Token t_char;
    private static Token t_arrow;
    private static Token t_macro;
    private static Token t_integer;
    private static Token t_float;
    private static Token t_comment;
    private static Token t_edocTag;
    private static Token t_htmlTag;
    private static Token t_tildeTag;
    private static Token t_escapeTag;

    protected final IColorManager fColorManager;
    protected List<ErlToken> fTokens;
    protected int fCrtToken;
    private int rangeLength;
    private int rangeOffset;

    public ErlCodeScanner(final IColorManager colorManager) {
        fColorManager = colorManager;
        setTokens();
    }

    private void setTokens() {
        if (t_string != null) {
            return;
        }
        t_string = new Token(getTextAttribute(TokenHighlight.STRING));
        t_tildeTag = new Token(getTextAttribute(TokenHighlight.TILDE_TAG));
        t_escapeTag = new Token(getTextAttribute(TokenHighlight.ESCAPE_TAG));
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
        t_edocTag = new Token(getTextAttribute(TokenHighlight.EDOC_TAG));
        t_htmlTag = new Token(getTextAttribute(TokenHighlight.HTML_TAG));
    }

    protected TextAttribute getTextAttribute(final TokenHighlight th) {
        final String qualifier = ColoringPreferencePage.COLORS_QUALIFIER
                + th.getName();
        final HighlightStyle data = new HighlightStyle();
        data.load(qualifier, th.getDefaultData());
        new InstanceScope().getNode(qualifier)
                .addPreferenceChangeListener(this);
        return new TextAttribute(fColorManager.getColor(data.getColor()), null,
                data.getStyle());
    }

    // private static final List<String> RESERVED = Arrays.asList(new String[] {
    // "after", "begin", "case", "try", "cond", "catch", "andalso",
    // "orelse", "end", "fun", "if", "let", "of", "query", "receive",
    // "when", "bnot", "not", "div", "rem", "band", "and", "bor", "bxor",
    // "bsl", "bsr", "or", "xor", "spec", });

    public IToken convert(final ErlToken tk) {
        if (tk == ErlToken.EOF) {
            return Token.EOF;
        }

        switch (tk.getKind()) {
        case ErlToken.KIND_STRING:
            return t_string;
        case ErlToken.KIND_ATOM:
            return t_atom;
        case ErlToken.KIND_VAR:
            return t_var;
        case ErlToken.KIND_CHAR:
            return t_char;
        case ErlToken.KIND_MACRO:
            return t_macro;
        case ErlToken.KIND_ARROW:
            return t_arrow;
        case ErlToken.KIND_INTEGER:
            return t_integer;
        case ErlToken.KIND_FLOAT:
            return t_float;
        case ErlToken.KIND_COMMENT:
            return t_comment;
        case ErlToken.KIND_KEYWORD:
            return t_keyword;
        default:
            return t_default;
        }
    }

    public void handleColorChange(final String id, final RGB newValue,
            final int style) {
        final Token token = getToken(id);
        fixTokenData(token, newValue, style);
    }

    public static Token getToken(final String id) {
        if (TokenHighlight.KEYWORD.getName().equals(id)) {
            return t_keyword;
        } else if (TokenHighlight.STRING.getName().equals(id)) {
            return t_string;
        } else if (TokenHighlight.TILDE_TAG.getName().equals(id)) {
            return t_tildeTag;
        } else if (TokenHighlight.ESCAPE_TAG.getName().equals(id)) {
            return t_escapeTag;
        } else if (TokenHighlight.VARIABLE.getName().equals(id)) {
            return t_var;
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
        } else if (TokenHighlight.COMMENT.getName().equals(id)) {
            return t_comment;
        } else if (TokenHighlight.EDOC_TAG.getName().equals(id)) {
            return t_edocTag;
        } else if (TokenHighlight.HTML_TAG.getName().equals(id)) {
            return t_htmlTag;
        }
        return t_default;
    }

    private void fixTokenData(final Token token, final RGB newValue,
            final int style) {
        final TextAttribute attr = (TextAttribute) token.getData();
        token.setData(new TextAttribute(fColorManager.getColor(newValue), attr
                .getBackground(), style));
    }

    @Override
    public void setRange(final IDocument document, final int offset,
            final int length) {
        if (document == null) {
            return;
        }
        try {
            rangeOffset = offset;
            rangeLength = length;

            // ErlLogger.debug("setRange %s %d:%d (%d:%d)", document,
            // rangeOffset,
            // rangeLength, offset, length);

            final String str = document.get(rangeOffset, rangeLength);
            setText(str);
        } catch (final BadLocationException e) {
            ErlLogger.warn(e);
        }

    }

    private void setText(final String text) {
        if (text == null) {
            return;
        }

        try {
            fCrtToken = -1;

            final String str = text;
            fTokens = ErlideScanner.lightScanString(str, rangeOffset);

        } catch (final BackendException e) {
            // e.printStackTrace();
        }
    }

    @Override
    public IToken nextToken() {
        final ErlToken nextErlToken = nextErlToken();
        return convert(nextErlToken);
    }

    @Override
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

    @Override
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
        if (tk.getOffset() >= rangeOffset + rangeLength) {
            return ErlToken.EOF;
        }
        return fTokens.get(fCrtToken);
    }

    @Override
    public void preferenceChange(final PreferenceChangeEvent event) {
        final String key = event.getKey();
        final Preferences node = event.getNode();
        final String newValue = (String) event.getNewValue();
        final Token tk = getToken(node.name());
        TextAttribute attr = (TextAttribute) tk.getData();
        if (HighlightStyle.COLOR_KEY.equals(key)) {
            if (newValue == null) {
                // color = dflt.color;
            } else {
                final Color color = fColorManager.getColor(StringConverter
                        .asRGB(newValue));
                attr = new TextAttribute(color, attr.getBackground(),
                        attr.getStyle());
            }
        } else if (HighlightStyle.STYLE_KEY.equals(key)) {
            if (newValue == null) {
                // style = dflt.style;
            } else {
                attr = new TextAttribute(attr.getForeground(),
                        attr.getBackground(), Integer.parseInt(newValue));
            }
        }
        tk.setData(attr);
    }

}
