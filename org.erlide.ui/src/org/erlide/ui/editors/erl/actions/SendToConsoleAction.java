package org.erlide.ui.editors.erl.actions;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IWorkbenchSite;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.console.ErlangConsolePage;

public class SendToConsoleAction extends SelectionDispatchAction {

	@Override
	public void run(final ITextSelection selection) {
		ErlangConsolePage p = ErlideUIPlugin.getDefault().getConsolePage();
		// make sure we have a console page to send it to
		if (p != null) {
			// try to make the text a full erlang expression, ending with dot
			String text = selection.getText().trim();
			if (text.endsWith(",") || text.endsWith(";")) {
				text = text.substring(0, text.length() - 1);
			}
			if (!text.endsWith(".")) {
				text += ".";
			}
			text += "\n";
			// send it off to the console
			p.input(text);
		}
		super.run(selection);
	}

	public SendToConsoleAction(final IWorkbenchSite site,
			final ResourceBundle bundle, final String prefix) {
		super(site);
		setText(getString(bundle, prefix + "label"));
		setToolTipText(getString(bundle, prefix + "tooltip"));
		setDescription(getString(bundle, prefix + "description"));
	}

	protected static String getString(final ResourceBundle bundle,
			final String key) {
		try {
			return bundle.getString(key);
		} catch (MissingResourceException x) {
		}
		return key;
	}

}
