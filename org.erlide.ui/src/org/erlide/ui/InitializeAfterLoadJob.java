/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.erlide.core.ErlangPlugin;

public class InitializeAfterLoadJob extends UIJob {

	public InitializeAfterLoadJob() {
		super("initializing erlang plugin");
		setSystem(true);
	}

	@Override
	public IStatus runInUIThread(final IProgressMonitor monitor) {
		ErlangPlugin.initializeAfterLoad(monitor);

		return new Status(IStatus.OK, ErlideUIPlugin.PLUGIN_ID, IStatus.OK,
				"", null); //$NON-NLS-1$
	}
}
