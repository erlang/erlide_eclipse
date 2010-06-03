/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.selection.internal;

import org.eclipse.core.resources.IFile;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;

public class ErlModuleSelection extends AbstractErlSelection {
	protected IErlModule module;

	public ErlModuleSelection(final IErlModule module, final IFile file) {
		this.module = module;
		this.file = file;
	}

	public IErlElement getErlElement() {
		return module;
	}

	public SelectionKind getDetailedKind() {
		return getKind();
	}

	public SelectionKind getKind() {
		return SelectionKind.MODULE;
	}

}
