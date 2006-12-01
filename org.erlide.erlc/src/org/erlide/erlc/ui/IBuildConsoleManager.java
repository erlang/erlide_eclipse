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
package org.erlide.erlc.ui;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.IDocument;
import org.erlide.erlc.core.resources.IConsole;

public interface IBuildConsoleManager {

	IConsole getConsole(IProject project);

	IDocument getConsoleDocument(IProject project);

	IProject getLastBuiltProject();

	void addConsoleListener(IBuildConsoleListener listener);

	void removeConsoleListener(IBuildConsoleListener listener);
}
