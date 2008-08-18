/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendListener;

public class BackendManagerPopup implements IBackendListener {

	private static final IBackendListener fInstance = new BackendManagerPopup();

	private static final int DELAY = 5000;

	private BackendManagerPopup() {
	}

	public static void init() {
		BackendManager.getDefault().addBackendListener(fInstance);
	}

	public void backendAdded(final IBackend b) {
		ErlLogger.debug("$$ added backend " + b.getInfo().getName());
		final IWorkbench workbench = PlatformUI.getWorkbench();
		final Display display = workbench.getDisplay();
		display.asyncExec(new Runnable() {

			public void run() {
				PopupDialog.showBalloon("Backend notification", "Added "
						+ b.getInfo().getName(), DELAY);
			}
		});
	}

	public void backendRemoved(final IBackend b) {
		ErlLogger.debug("$$ removed backend " + b.getInfo().getName());
		final IWorkbench workbench = PlatformUI.getWorkbench();
		final Display display = workbench.getDisplay();
		display.asyncExec(new Runnable() {

			public void run() {
				PopupDialog.showBalloon("Backend notification", "Removed "
						+ b.getInfo().getName(), DELAY);
			}
		});
	}
}
