package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.erlide.core.erlang.ErlScanner;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.IErlModule;

public final class ErlangWordFinder {

	public static IRegion findWord(final IErlModule module,
			final ErlangEditor editor, final int offset) {
		if (module == null) {
			return null;
		}
		if (editor != null) {
			editor.reconcileNow();
		}
		final ErlScanner scanner = module.getScanner();
		if (scanner == null) {
			return null;
		}
		final ErlToken token = scanner.getTokenAt(offset);
		if (token == null) {
			return null;
		}
		return new Region(token.getOffset(), token.getLength());
	}

}
