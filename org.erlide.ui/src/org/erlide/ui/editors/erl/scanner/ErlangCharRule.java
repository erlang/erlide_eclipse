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

    public IToken evaluate(final ICharacterScanner scanner) {
        int c = scanner.read();
        if (c == '$') {
            c = scanner.read();
            if (c != ICharacterScanner.EOF) {
                return token;
            }
        }
        scanner.unread();
        return Token.UNDEFINED;
    }

    public IToken getSuccessToken() {
        return token;
    }

    public IToken evaluate(final ICharacterScanner scanner, final boolean resume) {
        return evaluate(scanner);
    }

}
