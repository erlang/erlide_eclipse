package org.erlide.ui.internal.search;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.editors.erl.ErlangEditor;

public class ImplementorsSearchGroup extends ActionGroup {

    private static final String MENU_TEXT = "Declarations";

    private final IWorkbenchSite fSite;
    private ErlangEditor fEditor;
    private IActionBars fActionBars;

    private final String fGroupId;

    private FindImplementorsAction fFindImplementorsAction;
    private FindImplementorsInProjectAction fFindImplementorsInProjectAction;
    private FindImplementorsInWorkingSetAction fFindImplementorsInWorkingSetAction;

    /**
     * Creates a new <code>ReferencesSearchGroup</code>. The group requires that
     * the selection provided by the site's selection provider is of type <code>
     * org.eclipse.jface.viewers.IStructuredSelection</code> .
     * 
     * @param site
     *            the view part that owns this action group
     */
    public ImplementorsSearchGroup(final IWorkbenchSite site) {
        fSite = site;
        fGroupId = IContextMenuConstants.GROUP_SEARCH;

        fFindImplementorsAction = new FindImplementorsAction(site);
        fFindImplementorsAction
                .setActionDefinitionId("org.erlide.ui.search.implementors.in.workspace");

        fFindImplementorsInProjectAction = new FindImplementorsInProjectAction(
                site);
        fFindImplementorsInProjectAction
                .setActionDefinitionId("org.erlide.ui.search.implementors.in.project");

        fFindImplementorsInWorkingSetAction = new FindImplementorsInWorkingSetAction(
                site);
        fFindImplementorsInWorkingSetAction
                .setActionDefinitionId("org.erlide.ui.search.implementors.in.workset");

        // register the actions as selection listeners
        final ISelectionProvider provider = fSite.getSelectionProvider();
        final ISelection selection = provider.getSelection();
        registerAction(fFindImplementorsAction, provider, selection);
        registerAction(fFindImplementorsInProjectAction, provider, selection);
        registerAction(fFindImplementorsInWorkingSetAction, provider, selection);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     * 
     * @param editor
     *            the Erlang editor
     */
    public ImplementorsSearchGroup(final ErlangEditor editor) {
        Assert.isNotNull(editor);
        fEditor = editor;
        fSite = fEditor.getSite();
        fGroupId = ITextEditorActionConstants.GROUP_FIND;

        fFindImplementorsAction = new FindImplementorsAction(editor);
        fFindImplementorsAction
                .setActionDefinitionId("org.erlide.ui.search.implementors.in.workspace");
        fEditor.setAction(
                "SearchReferencesInWorkspace", fFindImplementorsAction); //$NON-NLS-1$

        fFindImplementorsInProjectAction = new FindImplementorsInProjectAction(
                fEditor);
        fFindImplementorsInProjectAction
                .setActionDefinitionId("org.erlide.ui.search.implementors.in.project");
        fEditor.setAction(
                "SearchImplementorsInProject", fFindImplementorsInProjectAction); //$NON-NLS-1$

        fFindImplementorsInWorkingSetAction = new FindImplementorsInWorkingSetAction(
                fEditor);
        fFindImplementorsInWorkingSetAction
                .setActionDefinitionId("org.erlide.ui.search.implementors.in.workset");
        fEditor.setAction("SearchImplementorsInWorkingSet",
                fFindImplementorsInWorkingSetAction);
    }

    private void registerAction(final SelectionDispatchAction action,
            final ISelectionProvider provider, final ISelection selection) {
        action.update(selection);
        provider.addSelectionChangedListener(action);
    }

    /**
     * Note: this method is for internal use only. Clients should not call this
     * method.
     * 
     * @return the menu label
     */
    protected String getName() {
        return MENU_TEXT;
    }

    @Override
    public void fillActionBars(final IActionBars actionBars) {
        Assert.isNotNull(actionBars);
        super.fillActionBars(actionBars);
        fActionBars = actionBars;
        updateGlobalActionHandlers();
    }

    private void addAction(final IAction action, final IMenuManager manager) {
        if (action.isEnabled()) {
            manager.add(action);
        }
    }

    private void addWorkingSetAction(final IWorkingSet[] workingSets,
            final IMenuManager manager) {
        FindAction action;
        if (fEditor != null) {
            action = new WorkingSetFindAction(fEditor,
                    new FindReferencesInWorkingSetAction(fEditor, workingSets),
                    SearchUtil.toString(workingSets));
        } else {
            action = new WorkingSetFindAction(fSite,
                    new FindReferencesInWorkingSetAction(fSite, workingSets),
                    SearchUtil.toString(workingSets));
        }
        final ActionContext context = getContext();
        if (context != null) {
            action.update(context.getSelection());
        }
        addAction(action, manager);
    }

    @Override
    public void fillContextMenu(final IMenuManager manager) {
        final MenuManager erlangSearchMM = new MenuManager(getName(),
                IContextMenuConstants.GROUP_SEARCH);
        addAction(fFindImplementorsAction, erlangSearchMM);
        addAction(fFindImplementorsInProjectAction, erlangSearchMM);

        erlangSearchMM.add(new Separator());

        for (final IWorkingSet[] i : SearchUtil.getLRUWorkingSets().getSorted()) {
            addWorkingSetAction(i, erlangSearchMM);
        }
        addAction(fFindImplementorsInWorkingSetAction, erlangSearchMM);

        if (!erlangSearchMM.isEmpty()) {
            manager.appendToGroup(fGroupId, erlangSearchMM);
        }
    }

    /*
     * Overrides method declared in ActionGroup
     */
    @Override
    public void dispose() {
        final ISelectionProvider provider = fSite.getSelectionProvider();
        if (provider != null) {
            disposeAction(fFindImplementorsAction, provider);
            disposeAction(fFindImplementorsInProjectAction, provider);
            disposeAction(fFindImplementorsInWorkingSetAction, provider);
        }
        fFindImplementorsAction = null;
        fFindImplementorsInProjectAction = null;
        fFindImplementorsInWorkingSetAction = null;
        updateGlobalActionHandlers();
        super.dispose();
    }

    private void updateGlobalActionHandlers() {
        if (fActionBars != null) {
            fActionBars.setGlobalActionHandler(
                    ErlideUIConstants.FIND_IMPLEMENTORS_IN_WORKSPACE,
                    fFindImplementorsAction);
            fActionBars.setGlobalActionHandler(
                    ErlideUIConstants.FIND_IMPLEMENTORS_IN_PROJECT,
                    fFindImplementorsInProjectAction);
            fActionBars.setGlobalActionHandler(
                    ErlideUIConstants.FIND_IMPLEMENTORS_IN_WORKING_SET,
                    fFindImplementorsInWorkingSetAction);
        }
    }

    private void disposeAction(final ISelectionChangedListener action,
            final ISelectionProvider provider) {
        if (action != null) {
            provider.removeSelectionChangedListener(action);
        }
    }

}
