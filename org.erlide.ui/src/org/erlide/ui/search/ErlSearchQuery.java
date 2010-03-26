package org.erlide.ui.search;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.search.ErlangElementRef;
import org.erlide.core.search.ErlangSearchElement;
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.runtime.backend.ErlideBackend;

import erlang.ErlideSearchServer;

public class ErlSearchQuery implements ISearchQuery {
	private final ErlangElementRef searchRef;
	// String module;
	// String fun;
	// int arity;
	// IErlElement element;
	private final List<IResource> scope;
	// private final int limitTo; // REFERENCES, DECLARATIONS or
	// ALL_OCCURRENCES
	private ErlangSearchResult fSearchResult;
	private List<ModuleLineFunctionArityRef> fResult;

	private String stateDirCached = null;

	public ErlSearchQuery(final ErlangElementRef ref,
			final List<IResource> scope) {
		searchRef = ref;
		this.scope = scope;
	}

	public boolean canRerun() {
		return true;
	}

	public boolean canRunInBackground() {
		return false;
	}

	public String getLabel() {
		return searchRef.toString();
	}

	public ISearchResult getSearchResult() {
		if (fSearchResult == null) {
			fSearchResult = new ErlangSearchResult(this);
		}
		return fSearchResult;
	}

	public IStatus run(final IProgressMonitor monitor)
			throws OperationCanceledException {
		final ErlideBackend backend = ErlangCore.getBackendManager()
				.getIdeBackend();
		fResult = ErlideSearchServer.findRefs(backend, searchRef, scope,
				getStateDir());
		// FIXME här ska vi se till att alla resurser (moduler) i scope läggs
		// in... ev portionera ut lite
		// TODO should be setScope
		final List<Match> l = new ArrayList<Match>(fResult.size());
		final List<ErlangSearchElement> result = new ArrayList<ErlangSearchElement>(
				fResult.size());
		for (final ModuleLineFunctionArityRef ref : fResult) {
			final Match m = SearchUtil.createMatch(ref);
			l.add(m);
			result.add((ErlangSearchElement) m.getElement());
		}
		fSearchResult.setResult(result);
		fSearchResult.addMatches(l.toArray(new Match[l.size()]));
		return Status.OK_STATUS;
	}

	private String getStateDir() {
		if (stateDirCached == null) {
			stateDirCached = ErlangPlugin.getDefault().getStateLocation()
					.toString();
		}
		return stateDirCached;
	}

	public ErlangElementRef getRef() {
		return searchRef;
	}
}
