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

	@Override
	public IErlElement getErlElement() {
		return module;
	}

	@Override
	public SelectionKind getDetailedKind() {
		return getKind();
	}

	@Override
	public SelectionKind getKind() {
		return SelectionKind.MODULE;
	}

}
