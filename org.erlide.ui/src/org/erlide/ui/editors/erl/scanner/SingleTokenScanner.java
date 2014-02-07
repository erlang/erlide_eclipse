package org.erlide.ui.editors.erl.scanner;

import org.eclipse.jface.text.rules.IToken;
import org.erlide.ui.util.IColorManager;

public class SingleTokenScanner extends ErlTokenScanner {

    public SingleTokenScanner(final IColorManager colorManager, final IToken token) {
        super(colorManager);
        setDefaultReturnToken(token);
    }
}
