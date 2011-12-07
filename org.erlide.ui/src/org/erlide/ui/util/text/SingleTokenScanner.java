package org.erlide.ui.util.text;

import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.IToken;
import org.erlide.ui.util.IColorManager;

public class SingleTokenScanner extends BufferedRuleBasedScanner {

    public SingleTokenScanner(final IColorManager manager, final IToken token) {
        super();
        setDefaultReturnToken(token);
    }
}
