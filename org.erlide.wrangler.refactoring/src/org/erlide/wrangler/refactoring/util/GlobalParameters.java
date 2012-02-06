/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.backend.BackendCore;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.internal.ErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.internal.ErlModuleSelection;
import org.erlide.wrangler.refactoring.selection.internal.ErlTextMemberSelection;

import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Class which point to singleton objects, like the actual editor, and has
 * functions which are 'global'.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class GlobalParameters {
    // TODO:: handle null exceptions
    static IEditorPart editor = null;

    static IErlSelection wranglerSelection = null;

    static boolean hasQuickCheck = false;
    static boolean isQCchecked = false;

    /**
     * Checks if QuickCheck is installed in the current machine.
     * 
     * @return true if QC is installed, else false
     */
    public static boolean hasQuickCheck() {
        if (isQCchecked) {
            return hasQuickCheck;
        } else {
            final RpcResult res = BackendCore.getBackendManager()
                    .getIdeBackend()
                    .call_noexception("code", "which", "a", "eqc");
            if (!res.isOk()) {
                return false;
            }
            if (res.getValue() instanceof OtpErlangString) {
                hasQuickCheck = true;
                isQCchecked = true;
            } else {
                isQCchecked = true;
                hasQuickCheck = false;
            }
            return hasQuickCheck;
        }
    }

    /**
     * Get the Eclipse default tab width
     * 
     * @return default tab width
     */
    public static int getTabWidth() {
        return 1;
    }

    /**
     * Stores the editor marked as 'current'
     * 
     * @param _editor
     *            stored editor
     */
    public static void setEditor(final IEditorPart _editor) {
        editor = _editor;
        wranglerSelection = new ErlTextMemberSelection((ITextEditor) editor);
    }

    /**
     * Get the actual Erlang selection in the workbench
     * 
     * @return selection in the workbench
     */
    public static IErlSelection getWranglerSelection() {
        return wranglerSelection;
    }

    /**
     * Stores a selection marked 'current'
     * 
     * @param selection
     *            Erlang selection
     */
    public static void setSelection(final ISelection selection)
            throws WranglerException {
        // TODO:: if the module is selected it is not handled
        try {
            if (editor == null) {
                final IWorkbench instance = PlatformUI.getWorkbench();
                final IWorkbenchWindow activeWorkbenchWindow = instance
                        .getActiveWorkbenchWindow();
                editor = activeWorkbenchWindow.getActivePage()
                        .getActiveEditor();
            }
            if (selection instanceof ITextSelection) {
                final IWorkbench instance = PlatformUI.getWorkbench();
                final IWorkbenchWindow activeWorkbenchWindow = instance
                        .getActiveWorkbenchWindow();
                editor = activeWorkbenchWindow.getActivePage()
                        .getActiveEditor();

                wranglerSelection = new ErlTextMemberSelection(
                        (ITextSelection) selection, (ITextEditor) editor);
            } else if (selection instanceof ITreeSelection) {
                final Object firstElement = ((ITreeSelection) selection)
                        .getFirstElement();
                if (firstElement instanceof IErlElement) {
                    final IErlElement element = (IErlElement) firstElement;
                    final IFile file = (IFile) element.getResource();
                    wranglerSelection = new ErlMemberSelection(element, file,
                            WranglerUtils.getDocument(file));
                } else if (firstElement instanceof IFile) {
                    final IFile file = (IFile) firstElement;
                    final IErlModule module =  ErlModelManager.getErlangModel().findModule(
                            file);
                    wranglerSelection = new ErlModuleSelection(module, file);
                } else {
                    wranglerSelection = null;
                    throw new WranglerException(
                            "Please select an Erlang element!");
                }
            } else {
                wranglerSelection = null;
                throw new WranglerException("Please select an Erlang element!");

            }
        } catch (final ClassCastException e) {
            e.printStackTrace();
        }

        /*
         * System.out.println(wranglerSelection.getStartLine() + "," +
         * wranglerSelection.getStartCol() + ";" +
         * wranglerSelection.getEndLine() + "," +
         * wranglerSelection.getEndCol());
         */

    }

    /**
     * Shows a yes or no question in a dialog.
     * 
     * @param s
     *            shell
     * @param question
     *            question which should be asked
     * @param title
     *            dilaog box title
     * @return true if the answer is yes, else false
     */
    public static boolean showDecidableQuestion(final Shell s,
            final String question, final String title) {
        boolean b;
        try {
            final MessageBox mb = new MessageBox(s, SWT.ICON_WARNING | SWT.YES
                    | SWT.NO);
            mb.setMessage(question);
            mb.setText(title);
            final int response = mb.open();
            if (response == SWT.YES) {
                b = true;
            } else {
                b = false;
            }
            return b;
        } catch (final Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    /**
     * Get the 'current' editor
     * 
     * @return actual editor
     */
    public static IEditorPart getEditor() {
        return editor;
    }
}
