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

/**
 * Helper for opening editors on the viewer's selection and link the selection
 * with the editor.
 * 
 * @since 3.5
 */
public abstract class OpenAndLinkWithEditorHelper {

    private final StructuredViewer viewer;

    private boolean isLinkingEnabled;

    private ISelection lastOpenSelection;

    private InternalListener listener;

    private final class InternalListener implements IOpenListener,
            ISelectionChangedListener, IDoubleClickListener {
        /*
         * @see
         * org.eclipse.jface.viewers.IOpenListener#open(org.eclipse.jface.viewers
         * .OpenEvent)
         */
        @Override
        public final void open(final OpenEvent event) {
            lastOpenSelection = event.getSelection();
            OpenAndLinkWithEditorHelper.this.open(lastOpenSelection,
                    OpenStrategy.activateOnOpen());
        }

        /*
         * @see
         * org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged
         * (org.eclipse.jface.viewers.SelectionChangedEvent)
         */
        @Override
        public void selectionChanged(final SelectionChangedEvent event) {
            final ISelection selection = event.getSelection();
            if (isLinkingEnabled && !selection.equals(lastOpenSelection)
                    && viewer.getControl().isFocusControl()) {
                linkToEditor(selection);
            }
            lastOpenSelection = null;
        }

        /*
         * @see
         * org.eclipse.jface.viewers.IDoubleClickListener#doubleClick(org.eclipse
         * .jface.viewers.DoubleClickEvent)
         */
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
     */
    public OpenAndLinkWithEditorHelper(final StructuredViewer viewer) {
        if (viewer == null) {
            throw new IllegalArgumentException(
                    "viewer can't be null in OpenAndLinkWithEditorHelper");
        }
        this.viewer = viewer;
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

    /**
     * Tells to activate the editor that is open on the given selection.
     * <p>
     * <strong>Note:</strong> The implementation must not open a new editor.
     * </p>
     * 
     * @param selection
     *            the viewer's selection
     * @since 3.5
     */
    protected abstract void activate(ISelection selection);

    /**
     * Tells to open an editor for the given selection.
     * 
     * @param selection
     *            the viewer's selection
     * @param activate
     *            <code>true</code> if the editor should be activated,
     *            <code>false</code> otherwise
     * @since 3.5
     */
    protected abstract void open(ISelection selection, boolean activate);

    /**
     * Tells to link the given selection to the editor that is open on the given
     * selection but does nothing if no matching editor can be found.
     * <p>
     * The common implementation brings that editor to front but more advanced
     * implementations may also select the given selection inside the editor.
     * </p>
     * <p>
     * <strong>Note:</strong> The implementation must not open a new editor.
     * </p>
     * 
     * @param selection
     *            the viewer's selection
     * @since 3.5
     */
    protected abstract void linkToEditor(ISelection selection);

}
