package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.swt.graphics.RGB;

public interface ErlTokenScanner extends ITokenScanner {

    void handleColorChange(final String id, final RGB newValue, final int style);

}
