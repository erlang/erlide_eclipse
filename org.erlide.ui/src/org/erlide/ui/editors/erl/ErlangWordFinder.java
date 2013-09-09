package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.parsing.ErlToken;

public final class ErlangWordFinder {

    public static IRegion findWord(final IErlModule module,
            final AbstractErlangEditor editor, final int offset) {
        if (module == null) {
            return null;
        }
        if (editor != null) {
            editor.reconcileNow();
            final ErlToken token = editor.getScanner().getTokenAt(offset);
            if (token == null) {
                return null;
            }
            return new Region(token.getOffset(), token.getLength());
        }
        return null;
    }

}
