/*******************************************************************************
 * Copyright (c) 2007 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.model.erlang;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.model.root.ISourceUnit;

public interface IErlangReconcilingListener {

    void aboutToBeReconciled();

    void reconciled(ISourceUnit ast, boolean forced,
            IProgressMonitor progressMonitor);

}
