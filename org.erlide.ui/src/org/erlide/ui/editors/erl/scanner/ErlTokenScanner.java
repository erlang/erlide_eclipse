package org.erlide.ui.editors.erl.scanner;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.erlide.engine.services.parsing.ErlToken;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.HighlightStyle;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;

public abstract class ErlTokenScanner extends BufferedRuleBasedScanner {

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

    public ErlTokenScanner(final IColorManager colorManager) {
        super();
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

    // private static final List<String> RESERVED = Arrays.asList(new String[] {
    // "after", "begin", "case", "try", "cond", "catch", "andalso",
    // "orelse", "end", "fun", "if", "let", "of", "query", "receive",
    // "when", "bnot", "not", "div", "rem", "band", "and", "bor", "bxor",
    // "bsl", "bsr", "or", "xor", "spec", });

    protected TextAttribute getTextAttribute(final TokenHighlight th) {
        final IPreferenceStore store = ErlideUIPlugin.getDefault().getPreferenceStore();
        final HighlightStyle data = th.getStyle(store);
        // load from prefsstore
        return new TextAttribute(fColorManager.getColor(data.getColor()), null,
                data.getStyles());
    }

    public void handleColorChange(final String id, final RGB newValue, final int style) {
        final Token token = getToken(id);
        fixTokenData(token, newValue, style);
    }

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

    private void fixTokenData(final Token token, final RGB color, final int style) {
        final TextAttribute attr = (TextAttribute) token.getData();
        final int newStyle = style == -1 ? attr.getStyle() : style;
        final Color newColor = color == null ? attr.getForeground() : fColorManager
                .getColor(color);
        token.setData(new TextAttribute(newColor, attr.getBackground(), newStyle));
    }

}
