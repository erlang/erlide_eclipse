/*******************************************************************************
 * Copyright (c) 2006, 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.debug.ui.utils;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.ui.texteditor.IMarkerUpdater;
import org.eclipse.ui.texteditor.MarkerUtilities;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.ErlDebugConstants;
import org.erlide.launch.debug.ErlangLineBreakpoint;
import org.erlide.launch.debug.IErlangBreakpoint;
import org.erlide.launch.debug.model.ErlangDebugTarget;

/**
 * This class provides a mechanism to correct the placement of a breakpoint
 * marker when the related document is edited.
 * 
 * This updater is used to cover the line number discrepancy cases that
 * <code>BasicMarkerUpdater</code> does not:
 * <ul>
 * <li>If you insert a blank line at the start of the line of code, the
 * breakpoint is moved from the blank line to the next viable line down,
 * following the same breakpoint placement rules as creating a breakpoint</li>
 * 
 * <li>If you select the contents of an entire line and delete them (leaving the
 * line blank), the breakpoint is moved to the next viable line down, following
 * the same breakpoint placement rules as creating a breakpoint</li>
 * 
 * <li>If the breakpoint is on the last viable line of a class file and the line
 * is removed via either of the aforementioned deletion cases, the breakpoint is
 * removed</li>
 * 
 * <li>If a line breakpoint would be moved to a valid method location with an
 * invalid line number it is removed, see {@link https
 * ://bugs.eclipse.org/bugs/show_bug.cgi?id=188676} for details</li>
 * 
 * <li>If a line breakpoint will be moved to a line that already has a line
 * breakpoint on it, the one being moved is removed, see {@link https
 * ://bugs.eclipse.org/bugs/show_bug.cgi?id=129066} for details</li>
 * 
 * <li>In the general deletion case if a valid breakpoint location can not be
 * determined, it is removed</li>
 * </ul>
 * 
 * @since 3.3
 */
public class BreakpointMarkerUpdater implements IMarkerUpdater {

    public BreakpointMarkerUpdater() {
    }

    @Override
    public String[] getAttribute() {
        return new String[] { IMarker.LINE_NUMBER };
    }

    @Override
    public String getMarkerType() {
        return "org.eclipse.debug.core.breakpointMarker"; //$NON-NLS-1$
    }

    @Override
    public boolean updateMarker(final IMarker marker, final IDocument document,
            final Position position) {
        if (position.isDeleted()) {
            return false;
        }
        try {
            final int line = MarkerUtilities.getLineNumber(marker);
            final int newLine = document.getLineOfOffset(position.getOffset()) + 1;
            if (line == newLine) {
                return true;
            }
            final IBreakpointManager manager = DebugPlugin.getDefault()
                    .getBreakpointManager();
            final IBreakpoint breakpoint = manager.getBreakpoint(marker);
            if (breakpoint == null) {
                return false;
            }
            if (breakpoint instanceof ErlangLineBreakpoint) {
                final ErlangLineBreakpoint erlangLineBreakpoint = (ErlangLineBreakpoint) breakpoint;
                final ErlangDebugTarget target = erlangLineBreakpoint
                        .getTarget();
                erlangLineBreakpoint.remove(target);
                MarkerUtilities.setLineNumber(marker, newLine);
                erlangLineBreakpoint.install(target);
                return true;
            }
            // if there exists a breakpoint on the line remove this one
            if (isLineBreakpointMarker(marker)) {
                ensureRanges(document, marker, line);
                return lineBreakpointExists(marker.getResource(), line, marker) == null;
            }
            // if the line info is a valid location with an invalid line
            // number,
            // a line breakpoint must be removed
            if (isLineBreakpointMarker(marker) && line == -1) {
                return false;
            }
            MarkerUtilities.setLineNumber(marker, line);
            if (isLineBreakpointMarker(marker)) {
                ensureRanges(document, marker, line);
            }
            return true;
        } catch (final BadLocationException e) {
            ErlLogger.error(e);
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
        return false;
    }

    private static boolean isLineBreakpointMarker(final IMarker marker)
            throws CoreException {
        return marker.getType() == ErlangLineBreakpoint.ERLANG_LINE_BREAKPOINT_MARKER_TYPE;
    }

    /**
     * Updates the charstart and charend ranges if necessary for the given line.
     * Returns immediately if the line is not valid (< 0 or greater than the
     * total line number count)
     * 
     * @param document
     * @param marker
     * @param line
     * @throws BadLocationException
     */
    private void ensureRanges(final IDocument document, final IMarker marker,
            final int line) throws BadLocationException {
        if (line < 0 || line > document.getNumberOfLines()) {
            return;
        }
        final IRegion region = document.getLineInformation(line - 1);
        final int charstart = region.getOffset();
        final int charend = charstart + region.getLength();
        MarkerUtilities.setCharStart(marker, charstart);
        MarkerUtilities.setCharEnd(marker, charend);
    }

    /**
     * Searches for an existing line breakpoint on the specified line in the
     * current type that does not match the id of the specified marker
     * 
     * @param resource
     *            the resource to care about
     * @param typeName
     *            the name of the type the breakpoint is in
     * @param lineNumber
     *            the number of the line the breakpoint is on
     * @param currentmarker
     *            the current marker we are comparing to see if it will be moved
     *            onto an existing one
     * @return an existing line breakpoint on the current line of the given
     *         resource and type if there is one
     * @throws CoreException
     * 
     * @since 3.4
     */
    private IErlangBreakpoint lineBreakpointExists(final IResource resource,
            final int lineNumber, final IMarker currentmarker)
            throws CoreException {
        final IBreakpointManager manager = DebugPlugin.getDefault()
                .getBreakpointManager();
        final IBreakpoint[] breakpoints = manager
                .getBreakpoints(ErlDebugConstants.ID_ERLANG_DEBUG_MODEL);
        final String markerType = currentmarker.getType();
        for (int i = 0; i < breakpoints.length; i++) {
            if (!(breakpoints[i] instanceof IErlangBreakpoint)) {
                continue;
            }
            final IErlangBreakpoint breakpoint = (IErlangBreakpoint) breakpoints[i];
            final IMarker marker = breakpoint.getMarker();
            if (marker != null && marker.exists()
                    && marker.getType().equals(markerType)
                    && currentmarker.getId() != marker.getId()) {
                if (marker instanceof ErlangLineBreakpoint) {
                    final ErlangLineBreakpoint erlangLineBreakpoint = (ErlangLineBreakpoint) marker;
                    if (erlangLineBreakpoint.getLineNumber() == lineNumber) {
                        return erlangLineBreakpoint;
                    }
                }
            }
        }
        return null;
    }

}
