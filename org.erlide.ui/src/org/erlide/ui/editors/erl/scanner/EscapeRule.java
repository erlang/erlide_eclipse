package org.erlide.ui.editors.erl.scanner;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

public class EscapeRule implements IPredicateRule {

    private final IToken token;

    public EscapeRule(final IToken token) {
        this.token = token;
    }

    @Override
    public IToken evaluate(final ICharacterScanner scanner) {
        int c = scanner.read();
        if (c == '\\') {
            c = scanner.read();
            if (c == 'x') {
                c = scanner.read();
                if (c == '{') {
                    c = scanner.read();
                    int hex = 0;
                    while (isHexChar(c)) {
                        hex++;
                        c = scanner.read();
                    }
                    if (c != '}') {
                        while (hex > 0) {
                            scanner.unread();
                            hex--;
                        }
                        scanner.unread();
                        scanner.unread();
                    }
                    return token;
                } else if (c != ICharacterScanner.EOF) {
                    int hex = 0;
                    while (isHexChar(c)) {
                        hex++;
                        c = scanner.read();
                    }
                    while (hex > 2) {
                        scanner.unread();
                        hex--;
                    }
                    if (hex > 0 && c != ICharacterScanner.EOF) {
                        scanner.unread();
                    }
                    return token;
                }
                return token;
            } else if (c != ICharacterScanner.EOF) {
                int octal = 0;
                while (c >= '0' && c <= '7') {
                    octal++;
                    c = scanner.read();
                }
                while (octal > 3) {
                    scanner.unread();
                    octal--;
                }
                if (octal > 0 && c != ICharacterScanner.EOF) {
                    scanner.unread();
                }
                return token;
            }
        }
        scanner.unread();
        return Token.UNDEFINED;
    }

    public boolean isHexChar(final int c) {
        return c >= '0' && c <= '7' || c >= 'a' && c <= 'f' || c >= 'A'
                && c <= 'F';
    }

    @Override
    public IToken getSuccessToken() {
        return token;
    }

    @Override
    public IToken evaluate(final ICharacterScanner scanner, final boolean resume) {
        return evaluate(scanner);
    }

}
