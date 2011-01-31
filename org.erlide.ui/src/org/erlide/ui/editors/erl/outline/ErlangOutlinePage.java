/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.ui.editors.erl.outline;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.commands.ActionHandler;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.WorkbenchAdapter;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.ErlideUIPluginImages;
import org.erlide.ui.actions.ActionMessages;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.ErlangSearchActionGroup;
import org.erlide.ui.actions.SortAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.navigator.ErlElementSorter;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.prefs.plugin.ErlEditorMessages;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.util.ProblemsLabelDecorator.ProblemsLabelChangedEvent;

/**
 * 
 * @author Vlad Dumitrescu
 * 
 */

public class ErlangOutlinePage extends ContentOutlinePage implements
        IErlModelChangeListener, ISortableContentOutlinePage {

    IErlModule fModule;
    private ErlangEditor fEditor;
    private CompositeActionGroup fActionGroups;
    private TreeViewer fOutlineViewer;
    private MemberFilterActionGroup fMemberFilterActionGroup;
    private SortAction fSortAction;
    private OpenAndLinkWithEditorHelper fOpenAndLinkWithEditorHelper;
    private ToggleLinkingAction fToggleLinkingAction;

    public class ErlangOutlineViewer extends TreeViewer {

        public ErlangOutlineViewer(final Tree tree) {
            super(tree);
            setAutoExpandLevel(0);
            setUseHashlookup(true);
        }

        /*
         * @see
         * ContentViewer#handleLabelProviderChanged(LabelProviderChangedEvent)
         */
        @Override
        protected void handleLabelProviderChanged(
                LabelProviderChangedEvent event) {
            final Object input = getInput();
            if (event instanceof ProblemsLabelChangedEvent) {
                final ProblemsLabelChangedEvent e = (ProblemsLabelChangedEvent) event;
                if (e.isMarkerChange() && input instanceof IErlModule) {
                    return; // marker changes can be ignored
                }
            }
            // look if the underlying resource changed
            final Object[] changed = event.getElements();
            if (changed != null) {
                final IResource resource = getUnderlyingResource();
                if (resource != null) {
                    for (int i = 0; i < changed.length; i++) {
                        if (changed[i] != null && changed[i].equals(resource)) {
                            // change event to a full refresh
                            event = new LabelProviderChangedEvent(
                                    (IBaseLabelProvider) event.getSource());
                            break;
                        }
                    }
                }
            }
            super.handleLabelProviderChanged(event);
        }

        private IResource getUnderlyingResource() {
            if (fModule != null) {
                return fModule.getResource();
            }
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#getControl()
     */
    @Override
    public Control getControl() {
        return fOutlineViewer.getControl();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.views.contentoutline.ContentOutlinePage#getSelection()
     */
    @Override
    public ISelection getSelection() {
        return fOutlineViewer.getSelection();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.views.contentoutline.ContentOutlinePage#getTreeViewer()
     */
    @Override
    protected TreeViewer getTreeViewer() {
        return fOutlineViewer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#setFocus()
     */
    @Override
    public void setFocus() {
        getControl().setFocus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.views.contentoutline.ContentOutlinePage#setSelection(org
     * .eclipse.jface.viewers.ISelection)
     */
    @Override
    public void setSelection(final ISelection selection) {
        fOutlineViewer.setSelection(selection);
    }

    /**
     * 
     * @param documentProvider
     * 
     * @param editor
     * 
     */
    public ErlangOutlinePage(final ErlangEditor editor) {
        // myDocProvider = documentProvider;
        fEditor = editor;
        ErlangCore.getModel().addModelChangeListener(this);
    }

    /**
     * 
     * @param editorInput
     * 
     */
    public void setInput(final IEditorInput editorInput) {
        // ErlLogger.log("> outline set input "+editorInput);
        fModule = null;
        try {
            fModule = ErlModelUtils.getModule(editorInput);
            if (fModule != null) {
                fModule.open(null);
            }
        } catch (final CoreException e) {
        }
    }

    public void refresh() {
        if (getTreeViewer() != null) {
            final Control c = getTreeViewer().getControl();
            if (c.isDisposed()) {
                return;
            }
            final Display d = c.getDisplay();
            d.asyncExec(new Runnable() {

                public void run() {
                    if (getTreeViewer().getControl() != null
                            && !getTreeViewer().getControl().isDisposed()) {
                        // ErlLogger.log("*>> refreshing.");
                        getTreeViewer().setInput(fModule);
                    }
                }
            });
        }
    }

    @Override
    public void createControl(final Composite parent) {
        // FIXME we still don't get a popup menu, we should have one...

        final Tree tree = new Tree(parent, SWT.MULTI);
        fOutlineViewer = new ErlangOutlineViewer(tree);
        fOutlineViewer.setContentProvider(fEditor
                .createOutlineContentProvider());
        fOutlineViewer.setLabelProvider(fEditor.createOutlineLabelProvider());
        fOutlineViewer.addPostSelectionChangedListener(this);
        fOutlineViewer.setInput(fModule);

        fOpenAndLinkWithEditorHelper = new OpenAndLinkWithEditorHelper(
                fOutlineViewer) {

            @Override
            protected void activate(final ISelection selection) {
                fEditor.doSelectionChanged(selection);
                getSite().getPage().activate(fEditor);
            }

            @Override
            protected void linkToEditor(final ISelection selection) {
                fEditor.doSelectionChanged(selection);
            }

            @Override
            protected void open(final ISelection selection,
                    final boolean activate) {
                fEditor.doSelectionChanged(selection);
                if (activate) {
                    getSite().getPage().activate(fEditor);
                }
            }

        };

        final IPageSite site = getSite();
        final IContextService service = (IContextService) site
                .getService(IContextService.class);
        service.activateContext("org.erlide.ui.erlangOutlineAndNavigatorScope");

        final MenuManager manager = new MenuManager();
        manager.setRemoveAllWhenShown(true);
        manager.addMenuListener(new IMenuListener() {
            public void menuAboutToShow(final IMenuManager m) {
                // recursive loop?
                // menuAboutToShow(m);
                contextMenuAboutToShow(m);
            }
        });
        final Menu menu = manager.createContextMenu(tree);
        tree.setMenu(menu);

        site.registerContextMenu(ErlangPlugin.PLUGIN_ID + ".outline", manager,
                fOutlineViewer);
        fActionGroups = new CompositeActionGroup(
                new ActionGroup[] { new ErlangSearchActionGroup(this) });
        // register global actions
        final IActionBars actionBars = site.getActionBars();
        actionBars.setGlobalActionHandler(ITextEditorActionConstants.UNDO,
                fEditor.getAction(ITextEditorActionConstants.UNDO));
        actionBars.setGlobalActionHandler(ITextEditorActionConstants.REDO,
                fEditor.getAction(ITextEditorActionConstants.REDO));
        fActionGroups.fillActionBars(actionBars);
        registerToolbarActions(actionBars);
        final IHandlerService handlerService = (IHandlerService) site
                .getService(IHandlerService.class);
        handlerService.activateHandler(
                IWorkbenchCommandConstants.NAVIGATE_TOGGLE_LINK_WITH_EDITOR,
                new ActionHandler(fToggleLinkingAction));
    }

    protected void contextMenuAboutToShow(final IMenuManager menu) {
        ErlideUIPlugin.createStandardGroups(menu);
        final IStructuredSelection selection = (IStructuredSelection) getSelection();
        fActionGroups.setContext(new ActionContext(selection));
        fActionGroups.fillContextMenu(menu);
    }

    public static class NoModuleElement extends WorkbenchAdapter implements
            IAdaptable {

        /*
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return ErlEditorMessages.ErlangOutlinePage_error_noelement;
        }

        /*
         * 
         * @see org.eclipse.core.runtime.IAdaptable#getAdapter(Class)
         */
        public Object getAdapter(@SuppressWarnings("rawtypes") final Class clas) {
            if (clas == IWorkbenchAdapter.class) {
                return this;
            }
            return null;
        }
    }

    public void select(final ISourceReference reference) {
        if (getTreeViewer() != null) {
            ISelection s = getTreeViewer().getSelection();
            if (s instanceof IStructuredSelection) {
                final IStructuredSelection ss = (IStructuredSelection) s;
                final List<?> elements = ss.toList();
                if (!elements.contains(reference)) {
                    s = reference == null ? StructuredSelection.EMPTY
                            : new StructuredSelection(reference);
                    getTreeViewer().setSelection(s, true);
                }
            }
        }
    }

    @Override
    public void dispose() {
        if (fEditor != null) {
            fEditor.outlinePageClosed();
            fEditor = null;
        }
        if (fMemberFilterActionGroup != null) {
            fMemberFilterActionGroup.dispose();
            fMemberFilterActionGroup = null;
        }
        ErlangCore.getModel().removeModelChangeListener(this);

        super.dispose();
    }

    public void elementChanged(final IErlElement element) {
        if (fModule == element) {
            refresh();
        }
    }

    /**
     * @param actionBars
     */
    private void registerToolbarActions(final IActionBars actionBars) {
        final IToolBarManager toolBarManager = actionBars.getToolBarManager();
        fSortAction = new SortAction(getTreeViewer(), "Sort",
                new ErlElementSorter(ErlElementSorter.SORT_ON_NAME),
                new ErlElementSorter(ErlElementSorter.SORT_ON_EXPORT), null,
                false, ErlideUIPlugin.getDefault().getPreferenceStore());
        toolBarManager.add(fSortAction);
        fMemberFilterActionGroup = new MemberFilterActionGroup(fOutlineViewer,
                "org.eclipse.jdt.ui.JavaOutlinePage"); //$NON-NLS-1$
        fMemberFilterActionGroup.contributeToToolBar(toolBarManager);

        final IMenuManager viewMenuManager = actionBars.getMenuManager();
        fToggleLinkingAction = new ToggleLinkingAction();
        fToggleLinkingAction
                .setActionDefinitionId(IWorkbenchCommandConstants.NAVIGATE_TOGGLE_LINK_WITH_EDITOR);
        viewMenuManager.add(fToggleLinkingAction);
    }

    public void sort(final boolean sorting) {
        ErlLogger.debug("sorting " + sorting);
    }

    /**
     * This action toggles whether this Java Outline page links its selection to
     * the active editor.
     * 
     * @since 3.0
     */
    public class ToggleLinkingAction extends Action {

        /**
         * Constructs a new action.
         */
        public ToggleLinkingAction() {
            super(ActionMessages.ToggleLinkingAction_label);
            setDescription(ActionMessages.ToggleLinkingAction_description);
            setToolTipText(ActionMessages.ToggleLinkingAction_tooltip);
            ErlideUIPluginImages.setLocalImageDescriptors(this, "synced.gif");
            PlatformUI.getWorkbench().getHelpSystem()
                    .setHelp(this, IErlangHelpContextIds.LINK_EDITOR_ACTION);
            final IEclipsePreferences prefsNode = MemberFilterActionGroup
                    .getPrefsNode();
            final boolean isLinkingEnabled = prefsNode.getBoolean(
                    PreferenceConstants.ERLANG_OUTLINE_LINK_WITH_EDITOR, true);
            setChecked(isLinkingEnabled);
            fOpenAndLinkWithEditorHelper.setLinkWithEditor(isLinkingEnabled);
        }

        /**
         * Runs the action.
         */
        @Override
        public void run() {
            final IEclipsePreferences prefsNode = MemberFilterActionGroup
                    .getPrefsNode();
            final boolean isChecked = isChecked();
            prefsNode.putBoolean(
                    PreferenceConstants.ERLANG_OUTLINE_LINK_WITH_EDITOR,
                    isChecked);
            if (isChecked && fEditor != null) {
                getTreeViewer().refresh();
            }
            fOpenAndLinkWithEditorHelper.setLinkWithEditor(isChecked);
        }

    }

}