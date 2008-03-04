/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.ui.console.FileLink;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.console.IHyperlink;
import org.eclipse.ui.console.IPatternMatchListenerDelegate;
import org.eclipse.ui.console.PatternMatchEvent;
import org.eclipse.ui.console.TextConsole;
import org.erlide.core.util.ResourceUtil;

public class ErlPatternMatchListenerDelegate implements
		IPatternMatchListenerDelegate {

	private TextConsole fConsole;

	public void connect(TextConsole console) {
		fConsole = console;
	}

	public void disconnect() {
		fConsole = null;
	}

	public void matchFound(PatternMatchEvent event) {
		if (fConsole == null) {
			return;
		}
		try {
			String txt = fConsole.getDocument().get(event.getOffset(),
					event.getLength());
			String[] v = txt.split(":");

			final IProject[] projects = ResourcesPlugin.getWorkspace()
					.getRoot().getProjects();
			IResource res = null;
			for (IProject prj : projects) {
				if (!prj.isOpen()) {
					continue;
				}
				try {
					res = ResourceUtil
							.recursiveFindNamedResourceWithReferences(prj, v[0]);
					if (res != null) {
						break;
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
			IFile file = null;
			if (res != null && res instanceof IFile) {
				file = (IFile) res;
			}
			IHyperlink link = new FileLink(file, null, -1, -1, Integer
					.parseInt(v[1]));
			fConsole.addHyperlink(link, event.getOffset(), event.getLength());
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

}
