package org.erlide.ui.internal.search;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.actions.ContributionItemFactory;
import org.eclipse.ui.dialogs.PropertyDialogAction;
import org.eclipse.ui.keys.IBindingService;
import org.eclipse.ui.part.Page;
import org.erlide.ui.editors.erl.ErlangEditor;

public class OpenViewActionGroup extends ActionGroup {

    private boolean fEditorIsOwner;
    // private boolean fIsTypeHiararchyViewerOwner;
    // private boolean fIsCallHiararchyViewerOwner;

    private ISelectionProvider fSelectionProvider;

    // private OpenSuperImplementationAction fOpenSuperImplementation;
    // private OpenExternalJavadocAction fOpenExternalJavadoc;
    // private OpenTypeHierarchyAction fOpenTypeHierarchy;
    // private OpenCallHierarchyAction fOpenCallHierarchy;
    private PropertyDialogAction fOpenPropertiesDialog;

    private boolean fShowOpenPropertiesAction = true;
    private boolean fShowShowInMenu = true;
    private IWorkbenchSite site;

    /**
     * Creates a new <code>OpenActionGroup</code>. The group requires that the
     * selection provided by the page's selection provider is of type
     * {@link IStructuredSelection}.
     * 
     * @param page
     *            the page that owns this action group
     */
    public OpenViewActionGroup(final Page page) {
        site = page.getSite();
        createSiteActions(null);
    }

    /**
     * Creates a new <code>OpenActionGroup</code>. The group requires that the
     * selection provided by the given selection provider is of type
     * {@link IStructuredSelection}.
     * 
     * @param page
     *            the page that owns this action group
     * @param selectionProvider
     *            the selection provider used instead of the page selection
     *            provider.
     * 
     * @since 3.2
     */
    public OpenViewActionGroup(final Page page,
            final ISelectionProvider selectionProvider) {
        site = page.getSite();
        createSiteActions(selectionProvider);
    }

    /**
     * Creates a new <code>OpenActionGroup</code>. The group requires that the
     * selection provided by the part's selection provider is of type
     * {@link IStructuredSelection}.
     * 
     * @param part
     *            the view part that owns this action group
     */
    public OpenViewActionGroup(final IViewPart part) {
        this(part, null);
    }

    /**
     * Creates a new <code>OpenActionGroup</code>. The group requires that the
     * selection provided by the given selection provider is of type
     * {@link IStructuredSelection}.
     * 
     * @param part
     *            the view part that owns this action group
     * @param selectionProvider
     *            the selection provider used instead of the page selection
     *            provider.
     * 
     * @since 3.2
     */
    public OpenViewActionGroup(final IViewPart part,
            final ISelectionProvider selectionProvider) {
        site = part.getSite();
        createSiteActions(selectionProvider);
        // we do a name check here to avoid class loading.
        // final String partName = part.getClass().getName();
        //		fIsTypeHiararchyViewerOwner= "org.eclipse.jdt.internal.ui.typehierarchy.TypeHierarchyViewPart".equals(partName); //$NON-NLS-1$
        //		fIsCallHiararchyViewerOwner= "org.eclipse.jdt.internal.ui.callhierarchy.CallHierarchyViewPart".equals(partName); //$NON-NLS-1$
    }

    /**
     * Creates a new <code>OpenActionGroup</code>. The group requires that the
     * selection provided by the given selection provider is of type
     * {@link IStructuredSelection}.
     * 
     * @param site
     *            the site that will own the action group.
     * @param selectionProvider
     *            the selection provider used instead of the page selection
     *            provider.
     * 
     * @since 3.2
     */
    public OpenViewActionGroup(final IWorkbenchSite site,
            final ISelectionProvider selectionProvider) {
        this.site = site;
        createSiteActions(selectionProvider);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     * 
     * @param part
     *            the editor part
     * 
     * @noreference This constructor is not intended to be referenced by
     *              clients.
     */
    public OpenViewActionGroup(final ErlangEditor part) {
        fEditorIsOwner = true;
        fShowShowInMenu = false;

        // fOpenSuperImplementation= new OpenSuperImplementationAction(part);
        // fOpenSuperImplementation.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_SUPER_IMPLEMENTATION);
        //		part.setAction("OpenSuperImplementation", fOpenSuperImplementation); //$NON-NLS-1$
        //
        // fOpenExternalJavadoc= new OpenExternalJavadocAction(part);
        // fOpenExternalJavadoc.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_EXTERNAL_JAVADOC);
        //		part.setAction("OpenExternalJavadoc", fOpenExternalJavadoc); //$NON-NLS-1$
        //
        // fOpenTypeHierarchy= new OpenTypeHierarchyAction(part);
        // fOpenTypeHierarchy.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_TYPE_HIERARCHY);
        //		part.setAction("OpenTypeHierarchy", fOpenTypeHierarchy); //$NON-NLS-1$
        //
        // fOpenCallHierarchy= new OpenCallHierarchyAction(part);
        // fOpenCallHierarchy.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_CALL_HIERARCHY);
        //        part.setAction("OpenCallHierarchy", fOpenCallHierarchy); //$NON-NLS-1$

        initialize(part.getEditorSite().getSelectionProvider());
    }

    /**
     * Specifies if this action group also contains the 'Properties' action (
     * {@link PropertyDialogAction}). By default, the action is contained in the
     * group.
     * 
     * @param enable
     *            If set, the 'Properties' action is part of this action group
     * @since 3.3
     */
    public void containsOpenPropertiesAction(final boolean enable) {
        fShowOpenPropertiesAction = enable;
    }

    /**
     * Specifies if this action group also contains the 'Show In' menu (See
     * {@link ContributionItemFactory#VIEWS_SHOW_IN}). By default, the action is
     * contained in the group except for editors.
     * 
     * @param enable
     *            If set, the 'Show In' menu is part of this action group
     * @since 3.3
     */
    public void containsShowInMenu(final boolean enable) {
        fShowShowInMenu = enable;
    }

    private void createSiteActions(final ISelectionProvider specialProvider) {
        // fOpenSuperImplementation= new OpenSuperImplementationAction(site);
        // fOpenSuperImplementation.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_SUPER_IMPLEMENTATION);
        // fOpenSuperImplementation.setSpecialSelectionProvider(specialProvider);
        //
        // fOpenExternalJavadoc= new OpenExternalJavadocAction(site);
        // fOpenExternalJavadoc.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_EXTERNAL_JAVADOC);
        // fOpenExternalJavadoc.setSpecialSelectionProvider(specialProvider);
        //
        // fOpenTypeHierarchy= new OpenTypeHierarchyAction(site);
        // fOpenTypeHierarchy.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_TYPE_HIERARCHY);
        // fOpenTypeHierarchy.setSpecialSelectionProvider(specialProvider);
        //
        // fOpenCallHierarchy= new OpenCallHierarchyAction(site);
        // fOpenCallHierarchy.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_CALL_HIERARCHY);
        // fOpenCallHierarchy.setSpecialSelectionProvider(specialProvider);

        final ISelectionProvider provider = specialProvider != null ? specialProvider
                : site.getSelectionProvider();

        fOpenPropertiesDialog = new PropertyDialogAction(site, provider);
        fOpenPropertiesDialog
                .setActionDefinitionId("org.eclipse.ui.file.properties"); // $NON-NLS-1$

        initialize(provider);
    }

    private void initialize(final ISelectionProvider provider) {
        fSelectionProvider = provider;
        final ISelection selection = fSelectionProvider.getSelection();
        // fOpenSuperImplementation.update(selection);
        // fOpenExternalJavadoc.update(selection);
        // fOpenTypeHierarchy.update(selection);
        // fOpenCallHierarchy.update(selection);
        if (!fEditorIsOwner) {
            if (fShowOpenPropertiesAction) {
                if (selection instanceof IStructuredSelection) {
                    fOpenPropertiesDialog
                            .selectionChanged((IStructuredSelection) selection);
                } else {
                    fOpenPropertiesDialog.selectionChanged(selection);
                }
            }
            // provider.addSelectionChangedListener(fOpenSuperImplementation);
            // provider.addSelectionChangedListener(fOpenExternalJavadoc);
            // provider.addSelectionChangedListener(fOpenTypeHierarchy);
            // provider.addSelectionChangedListener(fOpenCallHierarchy);
            // no need to register the open properties dialog action since it
            // registers itself
        }
    }

    @Override
    public void fillActionBars(final IActionBars actionBar) {
        super.fillActionBars(actionBar);
        setGlobalActionHandlers(actionBar);
    }

    @Override
    public void fillContextMenu(final IMenuManager menu) {
        super.fillContextMenu(menu);
        // if (!fIsTypeHiararchyViewerOwner)
        // appendToGroup(menu, fOpenTypeHierarchy);
        // if (!fIsCallHiararchyViewerOwner)
        // appendToGroup(menu, fOpenCallHierarchy);

        if (fShowShowInMenu) {
            final MenuManager showInSubMenu = new MenuManager(
                    getShowInMenuLabel());
            final IWorkbenchWindow workbenchWindow = getSite()
                    .getWorkbenchWindow();
            showInSubMenu.add(ContributionItemFactory.VIEWS_SHOW_IN
                    .create(workbenchWindow));
            menu.appendToGroup(IContextMenuConstants.GROUP_OPEN, showInSubMenu);
        }

        final IStructuredSelection selection = getStructuredSelection();
        if (fShowOpenPropertiesAction && selection != null
                && fOpenPropertiesDialog.isApplicableForSelection()) {
            menu.appendToGroup(IContextMenuConstants.GROUP_PROPERTIES,
                    fOpenPropertiesDialog);
        }
    }

    private IWorkbenchSite getSite() {
        return site;
    }

    public static final String NAVIGATE_SHOW_IN_QUICK_MENU = "org.eclipse.ui.navigate.showInQuickMenu"; //$NON-NLS-1$

    private String getShowInMenuLabel() {
        String keyBinding = null;

        final IBindingService bindingService = (IBindingService) PlatformUI
                .getWorkbench().getAdapter(IBindingService.class);
        if (bindingService != null) {
            keyBinding = bindingService
                    .getBestActiveBindingFormattedFor(NAVIGATE_SHOW_IN_QUICK_MENU);
        }

        if (keyBinding == null) {
            keyBinding = ""; //$NON-NLS-1$
        }

        return "Sho&w In" + '\t' + keyBinding;
    }

    /*
     * @see ActionGroup#dispose()
     */
    @Override
    public void dispose() {
        // fSelectionProvider
        // .removeSelectionChangedListener(fOpenSuperImplementation);
        // fSelectionProvider.removeSelectionChangedListener(fOpenExternalJavadoc);
        // fSelectionProvider.removeSelectionChangedListener(fOpenTypeHierarchy);
        // fSelectionProvider.removeSelectionChangedListener(fOpenCallHierarchy);
        super.dispose();
    }

    private void setGlobalActionHandlers(final IActionBars actionBars) {
        // actionBars.setGlobalActionHandler(
        // JdtActionConstants.OPEN_SUPER_IMPLEMENTATION,
        // fOpenSuperImplementation);
        // actionBars
        // .setGlobalActionHandler(
        // JdtActionConstants.OPEN_EXTERNAL_JAVA_DOC,
        // fOpenExternalJavadoc);
        // actionBars.setGlobalActionHandler(
        // JdtActionConstants.OPEN_TYPE_HIERARCHY, fOpenTypeHierarchy);
        // actionBars.setGlobalActionHandler(
        // JdtActionConstants.OPEN_CALL_HIERARCHY, fOpenCallHierarchy);

        if (!fEditorIsOwner && fShowOpenPropertiesAction) {
            actionBars.setGlobalActionHandler(ActionFactory.PROPERTIES.getId(),
                    fOpenPropertiesDialog);
        }
    }

    // private void appendToGroup(final IMenuManager menu, final IAction action)
    // {
    // if (action.isEnabled()) {
    // menu.appendToGroup(IContextMenuConstants.GROUP_OPEN, action);
    // }
    // }

    private IStructuredSelection getStructuredSelection() {
        final ISelection selection = getContext().getSelection();
        if (selection instanceof IStructuredSelection) {
            return (IStructuredSelection) selection;
        }
        return null;
    }
}
