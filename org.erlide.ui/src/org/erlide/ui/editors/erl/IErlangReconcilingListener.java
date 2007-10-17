/*******************************************************************************
 * Copyright (c) 2007 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.IErlModule;

public interface IErlangReconcilingListener {

	void aboutToBeReconciled();

	void reconciled(IErlModule ast, boolean forced,
			IProgressMonitor progressMonitor);

}
