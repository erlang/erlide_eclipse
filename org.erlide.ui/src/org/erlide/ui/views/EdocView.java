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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.search.ui.IContextMenuConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IAbstractTextEditorHelpContextIds;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.hover.ErlTextHover;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.internal.information.HandleEdocLinksLocationListener;
import org.erlide.ui.internal.information.OpenDeclarationAction;
import org.erlide.util.ErlLogger;
import org.osgi.framework.Bundle;

/**
 * View which shows Edoc for a given Erlang element.
 *
 */
public class EdocView extends AbstractInfoView {

    public static final String ID = "org.erlide.ui.views.EdocView";
    /**
     * Preference key for the preference whether to show a dialog when the SWT
     * Browser widget is not available.
     *
     * @since 3.0
     */
    //    private static final String DO_NOT_WARN_PREFERENCE_KEY = "EdocView.error.doNotWarn"; //$NON-NLS-1$

    // private static final boolean WARNING_DIALOG_ENABLED = true;

    private Browser fBrowser;

    /** The information presenter. */
    // private DefaultInformationControl.IInformationPresenterExtension
    // fPresenter;

    /** The text presentation. */
    // private final TextPresentation fPresentation = new TextPresentation();

    /** The select all action */
    private SelectAllAction fSelectAllAction;

    private static URL fgStyleSheet;
    private HandleEdocLinksLocationListener locationListener;
    private ErlBrowserInformationControlInput input;
    protected OpenDeclarationAction openDeclarationAction;

    /**
     * The Edoc view's select all action.
     */
    private static class SelectAllAction extends Action {

        /** The control. */
        private final Control fControl;

        /** The selection provider. */
        private final SelectionProvider fSelectionProvider;

        /**
         * Creates the action.
         *
         * @param control
         *            the widget
         * @param selectionProvider
         *            the selection provider
         */
        public SelectAllAction(final Control control,
                final SelectionProvider selectionProvider) {
            super("selectAll");

            assertThat(control, is(not(nullValue())));
            assertThat(selectionProvider, is(not(nullValue())));
            fControl = control;
            fSelectionProvider = selectionProvider;

            setText("Select All");
            setToolTipText("Select All");
            setDescription("Select All");

            PlatformUI.getWorkbench().getHelpSystem()
                    .setHelp(this, IAbstractTextEditorHelpContextIds.SELECT_ALL_ACTION);
        }

        /**
         * Selects all in the view.
         */
        @Override
        public void run() {
            if (fControl instanceof StyledText) {
                ((StyledText) fControl).selectAll();
            } else {
                // https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
                // ((Browser)fControl).selectAll();
                if (fSelectionProvider != null) {
                    fSelectionProvider.fireSelectionChanged();
                }
            }
        }
    }

    /**
     * The edoc view's selection provider.
     */
    private static class SelectionProvider implements ISelectionProvider {

        /** The selection changed listeners. */
        private final List<ISelectionChangedListener> fListeners = new ArrayList<ISelectionChangedListener>(
                0);

        /** The widget. */
        private final Control fControl;

        /**
         * Creates a new selection provider.
         *
         * @param control
         *            the widget
         */
        public SelectionProvider(final Control control) {
            assertThat(control, is(not(nullValue())));
            fControl = control;
            if (fControl instanceof StyledText) {
                ((StyledText) fControl).addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(final SelectionEvent e) {
                        fireSelectionChanged();
                    }
                });
            } else {
                // https://bugs.eclipse.org/bugs/show_bug.cgi?id=63022
                // ((Browser)fControl).addSelectionListener(new
                // SelectionAdapter() {
                // public void widgetSelected(SelectionEvent e) {
                // fireSelectionChanged();
                // }
                // });
            }
        }

        /**
         * Sends a selection changed event to all listeners.
         */
        public void fireSelectionChanged() {
            final ISelection selection = getSelection();
            final SelectionChangedEvent event = new SelectionChangedEvent(this, selection);
            final Object[] selectionChangedListeners = fListeners.toArray();
            for (final Object element : selectionChangedListeners) {
                ((ISelectionChangedListener) element).selectionChanged(event);
            }
        }

        /*
         * @see
         * org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener
         * (org.eclipse.jface.viewers.ISelectionChangedListener)
         */
        @Override
        public void addSelectionChangedListener(final ISelectionChangedListener listener) {
            fListeners.add(listener);
        }

        /*
         * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
         */
        @Override
        public ISelection getSelection() {
            if (fControl instanceof StyledText) {
                final IDocument document = new Document(
                        ((StyledText) fControl).getSelectionText());
                return new TextSelection(document, 0, document.getLength());
            }
            return StructuredSelection.EMPTY;
        }

        /*
         * @seeorg.eclipse.jface.viewers.ISelectionProvider#
         * removeSelectionChangedListener
         * (org.eclipse.jface.viewers.ISelectionChangedListener)
         */
        @Override
        public void removeSelectionChangedListener(
                final ISelectionChangedListener listener) {
            fListeners.remove(listener);
        }

        /*
         * @see
         * org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse
         * .jface.viewers.ISelection)
         */
        @Override
        public void setSelection(final ISelection selection) {
            // not supported
        }
    }

    /*
     * @see AbstractInfoView#internalCreatePartControl(Composite)
     */
    @Override
    protected void internalCreatePartControl(final Composite parent) {
        try {
            fBrowser = new Browser(parent, SWT.NONE);
            locationListener = new HandleEdocLinksLocationListener(this);
            fBrowser.addLocationListener(locationListener);
        } catch (final SWTError er) {
            ErlLogger.warn(er);
        }

        initStyleSheet();
        getViewSite().setSelectionProvider(new SelectionProvider(getControl()));
    }

    private void initStyleSheet() {
        final Bundle bundle = Platform.getBundle(ErlideUIPlugin.PLUGIN_ID);
        fgStyleSheet = bundle.getEntry("/edoc.css");
        if (fgStyleSheet != null) {

            try {
                fgStyleSheet = FileLocator.toFileURL(fgStyleSheet);
            } catch (final Exception e) {
            }
        }
    }

    /*
     * @see AbstractInfoView#createActions()
     */
    @Override
    protected void createActions() {
        super.createActions();
        openDeclarationAction = new OpenDeclarationAction(this);
        openDeclarationAction.setEnabled(false);
        fSelectAllAction = new SelectAllAction(getControl(), new SelectionProvider(
                getControl()));
    }

    @Override
    public void menuAboutToShow(final IMenuManager menu) {
        super.menuAboutToShow(menu);
        menu.appendToGroup(IContextMenuConstants.GROUP_OPEN, openDeclarationAction);
    }

    @Override
    protected void fillToolBar(final IToolBarManager tbm) {
        super.fillToolBar(tbm);
        tbm.add(openDeclarationAction);
    }

    @Override
    protected IAction getSelectAllAction() {
        return fSelectAllAction;
    }

    @Override
    protected void doSetInfo(final Object info) {
        super.doSetInfo(info);
        openDeclarationAction.setEnabled(true);
    }

    /*
     * @see AbstractInfoView#setForeground(Color)
     */
    @Override
    protected void setForeground(final Color color) {
        getControl().setForeground(color);
    }

    /*
     * @see AbstractInfoView#setBackground(Color)
     */
    @Override
    protected void setBackground(final Color color) {
        getControl().setBackground(color);
    }

    /*
     * @see AbstractInfoView#internalDispose()
     */
    @Override
    protected void internalDispose() {
        fBrowser = null;
    }

    /*
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        getControl().setFocus();
    }

    @Override
    public void setInfo(final Object info) {
        String edocHtml = null;
        if (info instanceof String) {
            edocHtml = (String) info;
        } else if (info instanceof ErlBrowserInformationControlInput) {
            final ErlBrowserInformationControlInput myInput = (ErlBrowserInformationControlInput) info;
            setInput(myInput);
            edocHtml = myInput.getHtml();
        }
        fBrowser.setText(edocHtml);
    }

    private void setInput(final ErlBrowserInformationControlInput input) {
        this.input = input;
    }

    public ErlBrowserInformationControlInput getInput() {
        return input;
    }

    public void setText(final String s) {
        setInfo(s);
    }

    /*
     * @see AbstractInfoView#getControl()
     */
    @Override
    protected Control getControl() {
        return fBrowser;
    }

    /*
     * @see
     * org.eclipse.jdt.internal.ui.infoviews.AbstractInfoView#getHelpContextId()
     *
     * @since 3.1
     */
    @Override
    protected String getHelpContextId() {
        return "";
    }

    @Override
    protected Object getInfoForSelection(final IWorkbenchPart part,
            final ISelection selection) {
        if (selection instanceof ITextSelection && part instanceof ErlangEditor) {
            final ITextSelection sel = (ITextSelection) selection;
            final ErlangEditor editor = (ErlangEditor) part;
            return ErlTextHover.getHoverInfoForOffset(sel.getOffset(), editor);
        }
        return null;
    }

}
