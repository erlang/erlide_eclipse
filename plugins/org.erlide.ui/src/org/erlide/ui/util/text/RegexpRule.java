package org.erlide.ui.util.text;

import java.util.regex.Pattern;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

public class RegexpRule implements IPredicateRule {

    private final Pattern pattern;
    private final IToken token;

    public RegexpRule(final Pattern pattern, final IToken token) {
        this.pattern = pattern;
        this.token = token;
    }

    public RegexpRule(final String regexp, final Token token) {
        pattern = Pattern.compile(regexp);
        this.token = token;
    }

    @Override
    public IToken getSuccessToken() {
        return token;
    }

    @Override
    public IToken evaluate(final ICharacterScanner scanner, final boolean resume) {
        final StringBuffer buffer = new StringBuffer();
        final char[][] lineDelimiters = scanner.getLegalLineDelimiters();

        int i = 1;
        int c = scanner.read();
        while (c != ICharacterScanner.EOF && !isEOL(c, lineDelimiters)) {
            buffer.append((char) c);
            if (pattern.matcher(buffer.toString()).matches()) {
                return token;
            }
            c = scanner.read();
            i++;
        }
        for (; i > 0; i--) {
            scanner.unread();
        }
        return Token.UNDEFINED;
    }

    @Override
    public IToken evaluate(final ICharacterScanner scanner) {
        return evaluate(scanner, false);
    }

    private boolean isEOL(final int c, final char[][] lineDelimiters) {
        for (int i = 0; i < lineDelimiters.length; i++) {
            if (c == lineDelimiters[i][0]) {
                return true;
            }
        }
        return false;
    }
}
