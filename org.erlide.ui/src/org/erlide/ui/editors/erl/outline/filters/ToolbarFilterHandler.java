package org.erlide.ui.editors.erl.outline.filters;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.views.contentoutline.ContentOutline;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.outline.ErlangOutlinePage;
import org.erlide.ui.editors.erl.outline.FilterDescriptor;
import org.osgi.service.prefs.BackingStoreException;

public class ToolbarFilterHandler extends AbstractHandler implements
        IElementUpdater {

    public ToolbarFilterHandler() {
        super();
    }

    public Object execute(final ExecutionEvent event) throws ExecutionException {
        // final Command command = event.getCommand();
        // final boolean oldValue = HandlerUtil.toggleCommandState(command);
        final String filterId = event.getParameter("org.erlide.ui.filterId");
        final FilterDescriptor desc = FilterDescriptor
                .getFilterDescriptor(filterId);
        final ViewerFilter filter = desc.getViewerFilter();
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        final boolean oldValue = prefsNode.getBoolean(filterId, false);
        final boolean value = !oldValue;
        final Object activePart = HandlerUtil.getVariable(event, "activePart");
        StructuredViewer viewer;
        if (activePart instanceof ContentOutline) {
            final ContentOutline outline = (ContentOutline) activePart;
            final ErlangOutlinePage erlangOutlinePage = (ErlangOutlinePage) outline
                    .getAdapter(ErlangOutlinePage.class);
            // shell = outline.getSite().getShell();
            // targetId = "org.eclipse.ui.views.ContentOutline";
            viewer = erlangOutlinePage.getTreeViewer();
            // } else if (activePart instanceof CommonNavigator) {
            // viewer = null;
        } else {
            return null;
        }
        if (value) {
            viewer.addFilter(filter);
        } else {
            viewer.removeFilter(filter);
        }
        prefsNode.putBoolean(filterId, value);
        try {
            prefsNode.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    public void updateElement(final UIElement element,
            @SuppressWarnings("rawtypes") final Map parameters) {
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        final String filterId = (String) parameters
                .get("org.erlide.ui.filterId");
        final boolean b = prefsNode.getBoolean(filterId, false);
        element.setChecked(b);
    }

}
