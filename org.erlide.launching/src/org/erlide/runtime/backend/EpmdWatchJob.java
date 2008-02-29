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
package org.erlide.runtime.backend;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.runtime.ErlangLaunchPlugin;

/**
 * Each 2 seconds, query epmd to see if there are any new nodes that have been
 * registered.
 * 
 */
public class EpmdWatchJob extends Job {

	public EpmdWatchJob() {
		super("Checking EPMD for new backends");
		setSystem(true);
		setPriority(SHORT);
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		if (ErlangLaunchPlugin.getDefault() == null) {
			return new Status(IStatus.CANCEL, ErlangLaunchPlugin.PLUGIN_ID,
					IStatus.OK, "", null); //$NON-NLS-1$
		}

		// BackendManager.getDefault().checkEpmd();

		this.schedule(2000);
		return Status.OK_STATUS;
	}
}
