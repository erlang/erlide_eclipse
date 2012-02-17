package org.erlide.ui.editors.erl.outline.filters;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.views.contentoutline.ContentOutline;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.outline.ErlangOutlinePage;
import org.osgi.service.prefs.BackingStoreException;

public class ToolbarFilterHandler extends AbstractHandler implements
        IElementUpdater {

    public ToolbarFilterHandler() {
        super();
    }

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        // final Command command = event.getCommand();
        // final boolean oldValue = HandlerUtil.toggleCommandState(command);
        final String filterId = event.getParameter("org.erlide.ui.filterId");
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        final boolean oldValue = prefsNode.getBoolean(filterId, false);
        final boolean value = !oldValue;
        final Object activePart = HandlerUtil.getVariable(event, "activePart");
        if (activePart instanceof ContentOutline) {
            OutlineFilterUtils.addFilter(filterId, value, activePart);
        }
        prefsNode.putBoolean(filterId, value);
        try {
            prefsNode.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    @Override
    public void updateElement(final UIElement element,
            @SuppressWarnings("rawtypes") final Map parameters) {
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        final String filterId = (String) parameters
                .get("org.erlide.ui.filterId");
        final boolean value = prefsNode.getBoolean(filterId, false);
        element.setChecked(value);
    }

}
