package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.core.model.erlang.IErlModule;

public final class ErlangWordFinder {

    public static IRegion findWord(final IErlModule module,
            final ErlangEditor editor, final int offset) {
        if (module == null) {
            return null;
        }
        if (editor != null) {
            editor.reconcileNow();
        }
        final ErlToken token = module.getScannerTokenAt(offset);
        if (token == null) {
            return null;
        }
        return new Region(token.getOffset(), token.getLength());
    }

}
