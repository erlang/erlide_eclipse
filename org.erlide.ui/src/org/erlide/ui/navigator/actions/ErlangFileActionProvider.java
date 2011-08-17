package org.erlide.ui.navigator.actions;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionConstants;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonMenuConstants;
import org.eclipse.ui.navigator.ICommonViewerSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;
import org.erlide.ui.actions.ErlangSearchActionGroup;

public class ErlangFileActionProvider extends CommonActionProvider {

    private OpenErlangAction openAction;
    private ErlangSearchActionGroup searchActionGroup;

    public ErlangFileActionProvider() {
    }

    @Override
    public void init(final ICommonActionExtensionSite aSite) {

        final ICommonViewerSite viewSite = aSite.getViewSite();
        if (viewSite instanceof ICommonViewerWorkbenchSite) {
            final ICommonViewerWorkbenchSite workbenchSite = (ICommonViewerWorkbenchSite) viewSite;
            final IWorkbenchPartSite site = workbenchSite.getSite();
            openAction = new OpenErlangAction(aSite,
                    workbenchSite.getSelectionProvider());
            searchActionGroup = new ErlangSearchActionGroup(site);
            final IContextService service = (IContextService) site
                    .getService(IContextService.class);
            service.activateContext("org.erlide.ui.erlangOutlineAndNavigatorScope");
        }
    }

    @Override
    public void fillActionBars(final IActionBars actionBars) {
        /* Set up the property open action when enabled. */
        if (openAction.isEnabled()) {
            actionBars.setGlobalActionHandler(ICommonActionConstants.OPEN,
                    openAction);
        }
    }

    @Override
    public void fillContextMenu(final IMenuManager menu) {
        if (openAction.isEnabled()) {
            menu.appendToGroup(ICommonMenuConstants.GROUP_OPEN, openAction);
        }
        searchActionGroup.fillContextMenu(menu);
    }

}
