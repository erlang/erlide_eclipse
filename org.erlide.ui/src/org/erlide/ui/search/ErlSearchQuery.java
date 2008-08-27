package org.erlide.ui.search;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.erlide.core.erlang.IErlElement;

public class ErlSearchQuery implements ISearchQuery {
	String module;
	String fun;
	int arity;
	IErlElement element;
	String[] scope;

	public ErlSearchQuery(final String module, final String fun,
			final int arity, final String[] scope) {
		this.module = module;
		this.fun = fun;
		this.arity = arity;
		this.scope = scope;
	}

	public ErlSearchQuery(final IErlElement element, final String[] scope) {
		this.element = element;
		this.scope = scope;
	}

	public boolean canRerun() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canRunInBackground() {
		// TODO Auto-generated method stub
		return false;
	}

	public String getLabel() {
		// TODO Auto-generated method stub
		return null;
	}

	public ISearchResult getSearchResult() {
		// TODO Auto-generated method stub
		return null;
	}

	public IStatus run(final IProgressMonitor monitor)
			throws OperationCanceledException {
		// TODO Auto-generated method stub
		return null;
	}

}
