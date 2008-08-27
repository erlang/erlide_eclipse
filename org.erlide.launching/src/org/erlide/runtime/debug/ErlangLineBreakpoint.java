/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.model.Breakpoint;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.erlide.runtime.backend.IBackend;

import erlang.ErlideDebug;

public class ErlangLineBreakpoint extends Breakpoint implements ILineBreakpoint {
	ErlangDebugTarget target = null;

	public ErlangLineBreakpoint() {
		super();
	}

	public String getModelIdentifier() {
		return IErlDebugConstants.ID_ERLANG_DEBUG_MODEL;
	}

	public void createMarker(final IResource resource, final int lineNumber)
			throws CoreException {
		final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				IMarker marker = resource
						.createMarker("org.erlide.launching.erlang.lineBreakpoint.marker");
				setMarker(marker);
				marker.setAttribute(IBreakpoint.ENABLED, Boolean.TRUE);
				marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
				marker.setAttribute(IBreakpoint.ID, getModelIdentifier());
				marker.setAttribute(IMarker.MESSAGE, "Line Breakpoint: "
						+ resource.getName() + " [line: " + lineNumber + "]");
			}
		};
		run(getMarkerRule(resource), runnable);
	}

	/**
	 * Installs this breakpoint
	 * 
	 * @param target
	 *            debug target
	 */
	public void install(final ErlangDebugTarget atarget) {
		this.target = atarget;
		createRequest(IErlDebugConstants.REQUEST_INSTALL);
	}

	private void createRequest(final int request) {
		final IBackend b = target.getBackend();
		int line = -1;
		try {
			line = getLineNumber();
		} catch (final CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
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

	public void remove(final ErlangDebugTarget atarget) {
		this.target = atarget;
		createRequest(IErlDebugConstants.REQUEST_REMOVE);
	}

}
