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
            final EscapeRule escape = new EscapeRule(token);
            final IToken tk = escape.evaluate(scanner);
            if (tk == Token.UNDEFINED) {
                c = scanner.read();
            }
            return token;
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
