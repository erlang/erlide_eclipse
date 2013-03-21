package org.erlide.ui.util.text;

import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.swt.graphics.RGB;
import org.erlide.ui.editors.erl.ErlTokenScanner;
import org.erlide.ui.util.IColorManager;

public class SingleTokenScanner extends BufferedRuleBasedScanner implements
        ErlTokenScanner {

    public SingleTokenScanner(final IColorManager manager, final IToken token) {
        super();
        setDefaultReturnToken(token);
    }

    @Override
    public void handleColorChange(final String id, final RGB newValue,
            final int style) {
        // TODO Auto-generated method stub

    }
}
