package org.erlide.ui.editors.erl.scanner;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

public class ErlangCharRule implements IPredicateRule {

    private final IToken token;

    public ErlangCharRule(final IToken token) {
        this.token = token;
    }

    @Override
    public IToken evaluate(final ICharacterScanner scanner) {
        int c = scanner.read();
        if (c == '$') {
            c = scanner.read();
            if (c == '\\') {
                c = scanner.read();
                if (c != ICharacterScanner.EOF) {
                    int octal = 0;
                    while (c >= '0' && c <= '7') {
                        octal++;
                        c = scanner.read();
                    }
                    while (octal > 3) {
                        scanner.unread();
                        octal--;
                    }
                    if (octal > 0) {
                        scanner.unread();
                    }
                    return token;
                }
            } else if (c != ICharacterScanner.EOF) {
                return token;
            }
        }
        scanner.unread();
        return Token.UNDEFINED;
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
