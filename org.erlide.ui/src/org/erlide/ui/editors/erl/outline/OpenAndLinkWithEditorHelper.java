/*******************************************************************************
 * Copyright (c) 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.erl.outline;

import org.eclipse.jface.util.OpenStrategy;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.ui.IWorkbenchPage;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * Helper for opening editors on the viewer's selection and link the selection
 * with the editor.
 */
public class OpenAndLinkWithEditorHelper {

    private final StructuredViewer viewer;
    private boolean isLinkingEnabled;
    private ISelection lastOpenSelection;
    private InternalListener listener;
    private final ErlangEditor fEditor;
    private final IWorkbenchPage page;

    private final class InternalListener implements IOpenListener,
            ISelectionChangedListener, IDoubleClickListener {

        @Override
        public final void open(final OpenEvent event) {
            lastOpenSelection = event.getSelection();
            OpenAndLinkWithEditorHelper.this.open(lastOpenSelection,
                    OpenStrategy.activateOnOpen());
        }

        @Override
        public void selectionChanged(final SelectionChangedEvent event) {
            final ISelection selection = event.getSelection();
            if (isLinkingEnabled && !selection.equals(lastOpenSelection)
                    && viewer.getControl().isFocusControl()) {
                linkToEditor(selection);
            }
            lastOpenSelection = null;
        }

        @Override
        public void doubleClick(final DoubleClickEvent event) {
            if (!OpenStrategy.activateOnOpen()) {
                activate(event.getSelection());
            }
        }

    }

    /**
     * Creates a new helper for the given viewer.
     *
     * @param viewer
     *            the viewer
     * @param iWorkbenchPage
     */
    public OpenAndLinkWithEditorHelper(final StructuredViewer viewer,
            final ErlangEditor fEditor, final IWorkbenchPage page) {
        if (viewer == null) {
            throw new IllegalArgumentException(
                    "viewer can't be null in OpenAndLinkWithEditorHelper");
        }
        this.viewer = viewer;
        this.fEditor = fEditor;
        this.page = page;
        listener = new InternalListener();
        viewer.addPostSelectionChangedListener(listener);
        viewer.addOpenListener(listener);
        viewer.addDoubleClickListener(listener);
    }

    /**
     * Sets whether editor that corresponds to the viewer's selection should be
     * brought to front.
     *
     * @param enabled
     *            <code>true</code> to enable, <code>false</code> to disable
     */
    public void setLinkWithEditor(final boolean enabled) {
        isLinkingEnabled = enabled;
    }

    /**
     * Disposes this helper.
     * <p>
     * Clients only need to call this method if their viewer has a longer
     * life-cycle than this helper.
     * </p>
     */
    public void dispose() {
        viewer.removePostSelectionChangedListener(listener);
        viewer.removeOpenListener(listener);
        viewer.removeDoubleClickListener(listener);
        listener = null;
    }

    private void activate(final ISelection selection) {
        fEditor.doSelectionChanged(selection);
        page.activate(fEditor);
    }

    private void linkToEditor(final ISelection selection) {
        fEditor.doSelectionChanged(selection);
    }

    private void open(final ISelection selection, final boolean activate) {
        fEditor.doSelectionChanged(selection);
        if (activate) {
            page.activate(fEditor);
        }
    }

    public boolean isLinkedWithEditor() {
        return isLinkingEnabled;
    }

}
