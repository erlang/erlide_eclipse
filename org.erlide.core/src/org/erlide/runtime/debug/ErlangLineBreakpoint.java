/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.model.Breakpoint;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.internal.ErlModelManager;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import erlang.ErlideDebug;

public class ErlangLineBreakpoint extends Breakpoint implements
        IErlangBreakpoint, ILineBreakpoint {

    public static final String ERLANG_LINE_BREAKPOINT_MARKER_TYPE = "org.erlide.core.erlang.lineBreakpoint.marker";

    private ErlangDebugTarget target;
    private String clauseHead;
    private int fHitCount;
    private int fBreakAction = BREAK_ACTION_BREAK;

    public ErlangLineBreakpoint() {
        super();
    }

    public String getModelIdentifier() {
        return ErlDebugConstants.ID_ERLANG_DEBUG_MODEL;
    }

    public void createMarker(final IResource resource, final int lineNumber)
            throws CoreException {
        final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
            public void run(final IProgressMonitor monitor)
                    throws CoreException {
                final IMarker marker = MarkerUtils
                        .createErlangLineBreakpointMarker(resource, lineNumber,
                                getModelIdentifier());
                setMarker(marker);
                resetClauseHead(lineNumber - 1, resource);
            }
        };
        run(getMarkerRule(resource), runnable);
    }

    protected void resetClauseHead(final int lineNumber,
            final IResource resource) {
        clauseHead = "";
        final IErlModel model = ErlModelManager.getDefault().getErlangModel();
        if (resource instanceof IFile) {
            final IFile file = (IFile) resource;
            final IErlModule m = model.findModule(file);
            if (m != null) {
                try {
                    m.open(null);
                    final IErlElement e = m.getElementAtLine(lineNumber);
                    if (e instanceof IErlFunctionClause) {
                        final IErlFunctionClause clause = (IErlFunctionClause) e;
                        clauseHead = clause.getName() + clause.getHead();
                    }
                } catch (final ErlModelException e1) {
                    ErlLogger.warn(e1);
                }
            }
        }
    }

    /**
     * Installs this breakpoint
     * 
     * @param theTarget
     *            debug target
     */
    public void install(final ErlangDebugTarget theTarget) {
        target = theTarget;
        if (theTarget != null) {
            createRequest(ErlDebugConstants.REQUEST_INSTALL);
        }
    }

    private void createRequest(final int request) {
        final Backend b = target.getBackend();
        int line = -1;
        try {
            line = getLineNumber();
        } catch (final CoreException e) {
            ErlLogger.warn(e);
        }
        final IResource r = getMarker().getResource();
        final String module = r.getLocation().toPortableString();
        if (line != -1) {
            ErlideDebug.addDeleteLineBreakpoint(b, module, line, request);
        }
    }

    public String getModule() {
        final IResource r = getMarker().getResource();
        return r.getFullPath().removeFileExtension().lastSegment();
    }

    // copied these three from LineBreakpoint, because I think we should have
    // class hierarchy around ErlangBreakpoint instead... (multiple inheritance,
    // anyone? =) )
    /**
     * @see ILineBreakpoint#getLineNumber()
     */
    public int getLineNumber() throws CoreException {
        final IMarker m = getMarker();
        if (m != null) {
            return m.getAttribute(IMarker.LINE_NUMBER, -1);
        }
        return -1;
    }

    /**
     * @see ILineBreakpoint#getCharStart()
     */
    public int getCharStart() throws CoreException {
        final IMarker m = getMarker();
        if (m != null) {
            return m.getAttribute(IMarker.CHAR_START, -1);
        }
        return -1;
    }

    /**
     * @see ILineBreakpoint#getCharEnd()
     */
    public int getCharEnd() throws CoreException {
        final IMarker m = getMarker();
        if (m != null) {
            return m.getAttribute(IMarker.CHAR_END, -1);
        }
        return -1;
    }

    public void remove(final ErlangDebugTarget theTarget) {
        target = theTarget;
        if (theTarget != null) {
            createRequest(ErlDebugConstants.REQUEST_REMOVE);
        }
    }

    public String getClauseHead() {
        if (clauseHead == null) {
            try {
                resetClauseHead(getLineNumber() - 1, getMarker().getResource());
            } catch (final CoreException e) {
            }
        }
        return clauseHead;
    }

    @Override
    public void setMarker(final IMarker marker) throws CoreException {
        super.setMarker(marker);
        clauseHead = null;
    }

    public String getCondition() throws CoreException {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean isConditionEnabled() throws CoreException {
        // TODO Auto-generated method stub
        return false;
    }

    public void setCondition(final String condition) throws CoreException {
        // TODO Auto-generated method stub

    }

    public void setConditionEnabled(final boolean enabled) throws CoreException {
        // TODO Auto-generated method stub

    }

    public boolean supportsCondition() {
        return !true; // FIXME
    }

    public void setHitCount(final int hitCount) {
        fHitCount = hitCount;
    }

    public int getHitCount() {
        return fHitCount;
    }

    public int getBreakAction() {
        return fBreakAction;
    }

    public void setBreakAction(final int traceAction) {
        fBreakAction = traceAction;
    }

    public ErlangDebugTarget getTarget() {
        return target;
    }

    public String getMessage() {
        final IMarker marker = getMarker();
        return "Line Breakpoint: " + marker.getResource().getName()
                + " [line: " + marker.getAttribute(IMarker.LINE_NUMBER, -1)
                + "]";
    }

}
