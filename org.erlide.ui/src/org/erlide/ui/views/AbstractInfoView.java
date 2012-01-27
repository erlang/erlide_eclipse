/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.views;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.internal.ErlideUIPlugin;

/**
 * Abstract class for views which show information for a given element.
 * 
 * @since 3.0
 */
abstract class AbstractInfoView extends ViewPart implements ISelectionListener,
        IMenuListener {

    /*
     * @see IPartListener2
     */
    private final IPartListener2 fPartListener = new IPartListener2() {

        @Override
        public void partVisible(final IWorkbenchPartReference ref) {
            if (ref.getId().equals(getSite().getId())) {
                final IWorkbenchPart activePart = ref.getPage().getActivePart();
                if (activePart != null) {
                    selectionChanged(activePart, ref.getPage().getSelection());
                }
                startListeningForSelectionChanges();
            }
        }

        @Override
        public void partHidden(final IWorkbenchPartReference ref) {
            if (ref.getId().equals(getSite().getId())) {
                stopListeningForSelectionChanges();
            }
        }

        @Override
        public void partInputChanged(final IWorkbenchPartReference ref) {
            if (!ref.getId().equals(getSite().getId())) {
                computeAndSetInput(ref.getPart(false));
            }
        }

        @Override
        public void partActivated(final IWorkbenchPartReference ref) {
        }

        @Override
        public void partBroughtToTop(final IWorkbenchPartReference ref) {
        }

        @Override
        public void partClosed(final IWorkbenchPartReference ref) {
        }

        @Override
        public void partDeactivated(final IWorkbenchPartReference ref) {
        }

        @Override
        public void partOpened(final IWorkbenchPartReference ref) {
        }
    };

    /** The current info. */
    protected String fCurrentViewInfo;

    /** The copy to clipboard action. */
    private SelectionDispatchAction fCopyToClipboardAction;

    /** The goto input action. */
    private GotoInputAction fGotoInputAction;

    /** Counts the number of background computation requests. */
    volatile int fComputeCount;

    /**
     * Set the input of this view.
     * 
     * @param input
     *            the input object
     */
    abstract protected void setInfo(String info);

    /**
     * Create the part control.
     * 
     * @param parent
     *            the parent control
     * @see IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    abstract protected void internalCreatePartControl(Composite parent);

    /**
     * Set the view's foreground color.
     * 
     * @param color
     *            the SWT color
     */
    abstract protected void setForeground(Color color);

    /**
     * Set the view's background color.
     * 
     * @param color
     *            the SWT color
     */
    abstract protected void setBackground(Color color);

    /**
     * Returns the view's primary control.
     * 
     * @return the primary control
     */
    abstract Control getControl();

    abstract protected String getInfoForSelection(IWorkbenchPart part,
            ISelection selection);

    /**
     * Returns the context ID for the Help system
     * 
     * @return the string used as ID for the Help context
     * @since 3.1
     */
    abstract protected String getHelpContextId();

    /*
     * @see IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public final void createPartControl(final Composite parent) {
        internalCreatePartControl(parent);
        setInfoColor();
        getSite().getWorkbenchWindow().getPartService()
                .addPartListener(fPartListener);
        createContextMenu();
        createActions();
        fillActionBars(getViewSite().getActionBars());
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(getControl(), getHelpContextId());
    }

    /**
     * Creates the actions and action groups for this view.
     */
    protected void createActions() {
        fGotoInputAction = new GotoInputAction(this);
        fGotoInputAction.setEnabled(false);
        fCopyToClipboardAction = new CopyToClipboardAction(getViewSite());

        final ISelectionProvider provider = getSelectionProvider();
        if (provider != null) {
            provider.addSelectionChangedListener(fCopyToClipboardAction);
        }
    }

    /**
     * Creates the context menu for this view.
     */
    protected void createContextMenu() {
        final MenuManager menuManager = new MenuManager("#PopupMenu"); //$NON-NLS-1$
        menuManager.setRemoveAllWhenShown(true);
        menuManager.addMenuListener(this);
        final Menu contextMenu = menuManager.createContextMenu(getControl());
        getControl().setMenu(contextMenu);
        getSite().registerContextMenu(menuManager, getSelectionProvider());
    }

    /*
     * @see IMenuListener#menuAboutToShow(org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void menuAboutToShow(final IMenuManager menu) {
        ErlideUIPlugin.createStandardGroups(menu);

        IAction action;

        action = getCopyToClipboardAction();
        if (action != null) {
            menu.appendToGroup(ITextEditorActionConstants.GROUP_EDIT, action);
        }

        action = getSelectAllAction();
        if (action != null) {
            menu.appendToGroup(ITextEditorActionConstants.GROUP_EDIT, action);
        }

        // TODO NYI menu.appendToGroup(IContextMenuConstants.GROUP_OPEN,
        // fGotoInputAction);
    }

    protected IAction getSelectAllAction() {
        return null;
    }

    protected IAction getCopyToClipboardAction() {
        return fCopyToClipboardAction;
    }

    /**
     * Returns the input of this view.
     * 
     * @return input the input object or <code>null</code> if not input is set
     */
    protected String getInfo() {
        return fCurrentViewInfo;
    }

    // Helper method
    ISelectionProvider getSelectionProvider() {
        return getViewSite().getSelectionProvider();
    }

    /**
     * Fills the actions bars.
     * <p>
     * Subclasses may extend.
     * 
     * @param actionBars
     *            the action bars
     */
    protected void fillActionBars(final IActionBars actionBars) {
        final IToolBarManager toolBar = actionBars.getToolBarManager();
        fillToolBar(toolBar);

        IAction action;

        action = getCopyToClipboardAction();
        if (action != null) {
            actionBars.setGlobalActionHandler(ActionFactory.COPY.getId(),
                    action);
        }

        action = getSelectAllAction();
        if (action != null) {
            actionBars.setGlobalActionHandler(ActionFactory.SELECT_ALL.getId(),
                    action);
        }
    }

    /**
     * Fills the tool bar.
     * <p>
     * Default is to do nothing.
     * </p>
     * 
     * @param tbm
     *            the tool bar manager
     */
    protected void fillToolBar(final IToolBarManager tbm) {
        // TODO NYI tbm.add(fGotoInputAction);
    }

    /**
     * Sets the foreground and background color to the corresponding SWT info
     * color.
     */
    private void setInfoColor() {
        if (getSite().getShell().isDisposed()) {
            return;
        }

        final Display display = getSite().getShell().getDisplay();
        if (display == null || display.isDisposed()) {
            return;
        }

        setForeground(display.getSystemColor(SWT.COLOR_INFO_FOREGROUND));
        setBackground(display.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
    }

    /**
     * Start to listen for selection changes.
     */
    protected void startListeningForSelectionChanges() {
        getSite().getWorkbenchWindow().getSelectionService()
                .addPostSelectionListener(this);
    }

    /**
     * Stop to listen for selection changes.
     */
    protected void stopListeningForSelectionChanges() {
        getSite().getWorkbenchWindow().getSelectionService()
                .removePostSelectionListener(this);
    }

    /*
     * @see ISelectionListener#selectionChanged(org.eclipse.ui.IWorkbenchPart,
     * org.eclipse.jface.viewers.ISelection)
     */
    @Override
    public void selectionChanged(final IWorkbenchPart part,
            final ISelection selection) {
        if (part.equals(this)) {
            return;
        }

        computeAndSetInput(part);
    }

    /**
     * Tells whether the new input should be ignored if the current input is the
     * same.
     * 
     * @return <code>true</code> if the new input should be ignored
     */
    protected boolean isIgnoringEqualInput() {
        return true;
    }

    // /**
    // * Finds and returns the Erlang element selected in the given part.
    // *
    // * @param part the workbench part for which to find the selected Erlang
    // element
    // * @param selection the selection
    // * @return the selected Erlang element
    // */
    // protected IErlElement findSelectedJavaElement(IWorkbenchPart part,
    // ISelection
    // selection) {
    // Object element;
    // try {
    // if (part instanceof ErlangEditor && selection instanceof ITextSelection)
    // {
    // IErlElement[] elements=
    // TextSelectionConverter.codeResolve((JavaEditor)part,
    // (ITextSelection)selection);
    // if (elements != null && elements.length > 0)
    // return elements[0];
    // else
    // return null;
    // } else if (selection instanceof IStructuredSelection) {
    // element= element= SelectionUtil.getSingleElement(selection);
    // } else {
    // return null;
    // }
    // } catch (JavaModelException e) {
    // return null;
    // }
    //
    // return findJavaElement(element);
    // }

    // /**
    // * Tries to get a Java element out of the given element.
    // *
    // * @param element an object
    // * @return the Java element represented by the given element or
    // <code>null</code>
    // */
    // private IErlElement findJavaElement(Object element) {
    //
    // if (element == null)
    // return null;
    //
    // IErlElement je= null;
    // if (element instanceof IAdaptable)
    // je= (IErlElement)((IAdaptable)element).getAdapter(IErlElement.class);
    //
    // return je;
    // }

    // /**
    // * Finds and returns the type for the given CU.
    // *
    // * @param cu the compilation unit
    // * @return the type with same name as the given CU or the first type in
    // the CU
    // */
    // protected IType getTypeForCU(ICompilationUnit cu) {
    //
    // if (cu == null || !cu.exists())
    // return null;
    //
    // // Use primary type if possible
    // IType primaryType= cu.findPrimaryType();
    // if (primaryType != null)
    // return primaryType;
    //
    // // Use first top-level type
    // try {
    // IType[] types= cu.getTypes();
    // if (types.length > 0)
    // return types[0];
    // else
    // return null;
    // } catch (JavaModelException ex) {
    // return null;
    // }
    // }

    /*
     * @see IWorkbenchPart#dispose()
     */
    @Override
    final public void dispose() {
        // cancel possible running computation
        fComputeCount--;

        getSite().getWorkbenchWindow().getPartService()
                .removePartListener(fPartListener);

        final ISelectionProvider provider = getSelectionProvider();
        if (provider != null) {
            provider.removeSelectionChangedListener(fCopyToClipboardAction);
        }

        internalDispose();
    }

    /*
     * @see IWorkbenchPart#dispose()
     */
    protected void internalDispose() {
    }

    /**
     * Determines all necessary details and delegates the computation into a
     * background thread.
     * 
     * @param part
     *            the workbench part
     */
    void computeAndSetInput(final IWorkbenchPart part) {

        final int currentCount = ++fComputeCount;

        final ISelectionProvider provider = part.getSite()
                .getSelectionProvider();
        if (provider == null) {
            return;
        }

        final ISelection selection = provider.getSelection();
        if (selection == null || selection.isEmpty()) {
            return;
        }

        final Thread thread = new Thread("Info view input computer") { //$NON-NLS-1$

            @Override
            public void run() {
                if (currentCount != fComputeCount) {
                    return;
                }

                // final IErlElement je= findSelectedJavaElement(part,
                // selection);
                final String info = getInfoForSelection(part, selection);
                if (info == null || info.length() == 0) {
                    return;
                }
                final Shell shell = getSite().getShell();
                if (shell.isDisposed()) {
                    return;
                }

                final Display display = shell.getDisplay();
                if (display.isDisposed()) {
                    return;
                }

                display.asyncExec(new Runnable() {

                    /*
                     * @see java.lang.Runnable#run()
                     */
                    @Override
                    public void run() {

                        if (fComputeCount != currentCount
                                || getViewSite().getShell().isDisposed()) {
                            return;
                        }

                        fCurrentViewInfo = info;
                        doSetInfo(info);
                    }
                });
            }
        };

        thread.setDaemon(true);
        thread.setPriority(Thread.MIN_PRIORITY);
        thread.start();
    }

    void doSetInfo(final String info) {
        setInfo(info);

        fGotoInputAction.setEnabled(true);

        // TODO setContentDescription
        // TODO setTitleToolTip
    }
}
