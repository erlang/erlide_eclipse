package org.erlide.wrangler.refactoring.selection.internal;

import org.eclipse.core.resources.IFile;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;

public class ErlModuleSelection extends AbstractErlSelection {
	protected IErlModule module;

	public ErlModuleSelection(IErlModule module, IFile file) {
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
