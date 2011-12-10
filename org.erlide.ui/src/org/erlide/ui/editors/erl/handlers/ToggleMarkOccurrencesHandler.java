package org.erlide.ui.editors.erl.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.ui.internal.ErlideUIPlugin;

public class ToggleMarkOccurrencesHandler extends AbstractHandler implements
        IHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final boolean oldValue = HandlerUtil.toggleCommandState(event
                .getCommand());
        final IEclipsePreferences prefsNode = ErlideUIPlugin.getPrefsNode();
        prefsNode.putBoolean("markingOccurences", !oldValue);
        return null;
    }

}
