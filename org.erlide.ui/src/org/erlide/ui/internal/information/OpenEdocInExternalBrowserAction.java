/*******************************************************************************
 * Copyright (c) 2000, 2009 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.information;

import java.net.URL;

import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchSite;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.ui.actions.ActionMessages;
import org.erlide.ui.actions.SelectionDispatchAction;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.internal.ErlBrowserInformationControlInput;

/**
 * This action opens the selected element's Javadoc in a browser as defined by
 * the preferences.
 * <p>
 * The action is applicable to selections containing elements of type
 * <code>IJavaElement</code>.
 *
 * @since 3.6
 * @noextend This class is not intended to be subclassed by clients.
 */
public class OpenEdocInExternalBrowserAction extends SelectionDispatchAction {

    private AbstractErlangEditor editor;

    private Shell fShell;

    private ErlBrowserInformationControlInput input;

    /**
     * Creates a new <code>OpenAttachedJavadocAction</code>. The action requires
     * that the selection provided by the site's selection provider is of type
     * <code>
     * org.eclipse.jface.viewers.IStructuredSelection</code> .
     *
     * @param site
     *            the site providing additional context information for this
     *            action
     */
    public OpenEdocInExternalBrowserAction(final IWorkbenchSite site,
            final ErlBrowserInformationControlInput input) {
        super(site);
        this.input = input;
        setText(ActionMessages.OpenEdocInExternalBrowser_label);
        setDescription(ActionMessages.OpenEdocInExternalBrowser_description);
        setToolTipText(ActionMessages.OpenEdocInExternalBrowser_tooltip);
        setEnabled(input != null);
        // PlatformUI
        // .getWorkbench()
        // .getHelpSystem()
        // .setHelp(this, IJavaHelpContextIds.OPEN_ATTACHED_JAVADOC_ACTION);
    }

    /**
     * Note: This constructor is for internal use only. Clients should not call
     * this constructor.
     *
     * @param editor
     *            the Java editor
     * @noreference This constructor is not intended to be referenced by
     *              clients.
     */
    public OpenEdocInExternalBrowserAction(final ErlangEditor editor) {
        this(editor.getEditorSite(), null);
        this.editor = editor;
        final ITextSelection selection = (ITextSelection) editor.getSelection();
        final IErlElement element = editor.getElementAt(selection.getOffset(), false);
        setEnabled(element != null);
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void selectionChanged(final ITextSelection selection) {
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void selectionChanged(final IStructuredSelection selection) {
        setEnabled(canEnableFor(selection));
    }

    /**
     * Tells whether this action can be enabled for the given selection.
     *
     * @param selection
     *            the structured selection.
     * @return <code>true</code> if the action can be enabled,
     *         <code>false</code> otherwise
     */
    protected boolean canEnableFor(final IStructuredSelection selection) {
        if (input != null) {
            return true;
        }
        if (selection.size() != 1) {
            return false;
        }
        return selection.getFirstElement() instanceof IErlElement;
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void run(final ITextSelection selection) {
        final IErlElement element = editor.getElementAt(selection.getOffset(), true);

        if (element != null) {
            run(element);
        }
    }

    /*
     * (non-Javadoc) Method declared on SelectionDispatchAction.
     */
    @Override
    public void run(final IStructuredSelection selection) {
        if (input != null) {
            final URL url = input.getDocumentationURL();
            if (url != null) {
                open(url);
            }
            return;
        }
        if (!canEnableFor(selection)) {
            return;
        }
        final IErlElement element = (IErlElement) selection.getFirstElement();
        run(element);
    }

    /**
     * Executes this actions with the given Java element.
     *
     * @param element
     *            the Java element
     */
    protected void run(final IErlElement element) {
        if (element == null) {
            return;
        }
        // final URL baseURL = HoverUtils.getBaseDocumentationURL(element);
        final URL url = HoverUtil.getDocumentationLocation(element);
        if (url != null) {
            open(url);
        }
    }

    /**
     * Opens the given URL in the browser.
     *
     * @param url
     *            the URL
     */
    protected void open(final URL url) {
        if (forceExternalBrowser()) {
            OpenBrowserUtil.openExternal(url, getShell().getDisplay());
        } else {
            OpenBrowserUtil.open(url, getShell().getDisplay());
        }
    }

    /**
     * Tells whether to use an external browser or the one chosen by the
     * preferences.
     *
     * @return <code>true</code> if it should always use the external browser,
     *         <code>false</code> to use the browser chosen in the preferences
     * @since 3.6
     */
    boolean forceExternalBrowser() {
        return false;
    }

    // private static void showMessage(final Shell shell, final String message,
    // final boolean isError) {
    // DisplayUtils.asyncExec(new Runnable() {
    // @Override
    // public void run() {
    // if (isError) {
    // MessageDialog.openError(shell, getTitle(), message);
    // } else {
    // MessageDialog.openInformation(shell, getTitle(), message);
    // }
    // }
    // });
    // }

    private static String getTitle() {
        return ActionMessages.OpenEdocInExternalBrowser_title;
    }

    /**
     * Note: this method is for internal use only. Clients should not call this
     * method.
     *
     * @return the dialog default title
     *
     * @noreference This method is not intended to be referenced by clients.
     */
    protected String getDialogTitle() {
        return getTitle();
    }

    /**
     * Returns the shell provided by the site owning this action.
     *
     * @return the site's shell
     */
    @Override
    public Shell getShell() {
        if (fShell != null) {
            return fShell;
        }
        return super.getShell();
    }

    public void setInput(final Object newInput) {
        if (newInput instanceof ErlBrowserInformationControlInput) {
            input = (ErlBrowserInformationControlInput) newInput;
        }
    }

    @Override
    public void setEnabled(final boolean enabled) {
        super.setEnabled(enabled);
    }

}
