/*******************************************************************************
 * Copyright (c) 2002, 2004 QNX Software Systems and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * QNX Software Systems - Initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.internal.ui.buildconsole;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.core.resources.ConsoleOutputStream;
import org.erlide.erlc.core.resources.IConsole;
import org.erlide.erlc.ui.IBuildConsoleManager;

public class ErlcBuildConsole implements IConsole {

	IProject fProject;

	IBuildConsoleManager fConsoleManager;

	/**
	 * Constructor for BuildConsole.
	 */
	public ErlcBuildConsole() {
		fConsoleManager = ErlideErlcPlugin.getDefault().getConsoleManager();
	}

	public void start(IProject project) {
		fProject = project;
		fConsoleManager.getConsole(project).start(project);
	}

	/**
	 * @throws CoreException
	 * @see org.eclipse.cdt.core.resources.IConsole#getOutputStream()
	 */
	public ConsoleOutputStream getOutputStream() throws CoreException {
		return fConsoleManager.getConsole(fProject).getOutputStream();
	}

	public ConsoleOutputStream getInfoStream() throws CoreException {
		return fConsoleManager.getConsole(fProject).getInfoStream();
	}

	public ConsoleOutputStream getErrorStream() throws CoreException {
		return fConsoleManager.getConsole(fProject).getErrorStream();
	}
}
