/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.launch.debug;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.model.IBreakpoint;

public class DebugMarkerUtils {

    public static IMarker createErlangLineBreakpointMarker(
            final IResource resource, final int lineNumber,
            final String modelIdentifier) throws CoreException {
        final IMarker marker = resource
                .createMarker(ErlangLineBreakpoint.ERLANG_LINE_BREAKPOINT_MARKER_TYPE);
        marker.setAttribute(IBreakpoint.ENABLED, Boolean.TRUE);
        marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
        marker.setAttribute(IBreakpoint.ID, modelIdentifier);
        marker.setAttribute(IMarker.MESSAGE,
                "Line Breakpoint: " + resource.getName() + " [line: "
                        + lineNumber + "]");
        return marker;
    }

}
