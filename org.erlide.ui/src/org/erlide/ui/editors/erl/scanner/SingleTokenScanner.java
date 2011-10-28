package org.erlide.ui.editors.erl.scanner;

import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.erlide.ui.util.IColorManager;

public class SingleTokenScanner extends BufferedRuleBasedScanner {

    public SingleTokenScanner(final IColorManager manager, final String hToken) {
        super();
        final Token token = ErlCodeScanner.getToken(hToken);
        setDefaultReturnToken(token);
    }
}
