/*******************************************************************************
 * Copyright (c) 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Bjorn Freeman-Benson - initial API and implementation
 *******************************************************************************/
package org.erlide.debug.ui.model;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.launch.debug.ErlangLineBreakpoint;
import org.erlide.ui.editors.erl.ErlangEditor;

public class ErlLineBreakpointAdapter implements IToggleBreakpointsTarget {
    @Override
    public void toggleLineBreakpoints(final IWorkbenchPart part,
            final ISelection selection) throws CoreException {
        final ITextEditor textEditor = getEditor(part);
        if (textEditor != null) {
            final IResource resource = (IResource) textEditor.getEditorInput()
                    .getAdapter(IResource.class);
            final ITextSelection textSelection = (ITextSelection) selection;
            final int lineNumber = textSelection.getStartLine();
            final IBreakpoint[] breakpoints = DebugPlugin.getDefault()
                    .getBreakpointManager()
                    .getBreakpoints(ErlDebugConstants.ID_ERLANG_DEBUG_MODEL);
            for (final IBreakpoint breakpoint : breakpoints) {
                if (resource.equals(breakpoint.getMarker().getResource())) {
                    if (breakpoint instanceof ILineBreakpoint) {
                        final ILineBreakpoint lineBr = (ILineBreakpoint) breakpoint;
                        if (lineBr.getLineNumber() == lineNumber + 1) {
                            breakpoint.delete();
                            return;
                        }
                    }
                }
            }
            // create line breakpoint (doc line numbers start at 0)
            final ErlangLineBreakpoint lineBreakpoint = new ErlangLineBreakpoint();
            lineBreakpoint.createMarker(resource, lineNumber + 1);
            DebugPlugin.getDefault().getBreakpointManager()
                    .addBreakpoint(lineBreakpoint);
        }
    }

    @Override
    public boolean canToggleLineBreakpoints(final IWorkbenchPart part,
            final ISelection selection) {
        return getEditor(part) != null;
    }

    /**
     * Returns the editor being used to edit a PDA file, associated with the
     * given part, or <code>null</code> if none.
     * 
     * @param part
     *            workbench part
     * @return the editor being used to edit a PDA file, associated with the
     *         given part, or <code>null</code> if none
     */
    private ITextEditor getEditor(final IWorkbenchPart part) {
        if (part instanceof ErlangEditor) {
            return (ITextEditor) part;
        }
        return null;
    }

    @Override
    public void toggleMethodBreakpoints(final IWorkbenchPart part,
            final ISelection selection) throws CoreException {
    }

    @Override
    public boolean canToggleMethodBreakpoints(final IWorkbenchPart part,
            final ISelection selection) {
        return false;
    }

    @Override
    public void toggleWatchpoints(final IWorkbenchPart part,
            final ISelection selection) throws CoreException {
    }

    @Override
    public boolean canToggleWatchpoints(final IWorkbenchPart part,
            final ISelection selection) {
        return false;
    }
}
