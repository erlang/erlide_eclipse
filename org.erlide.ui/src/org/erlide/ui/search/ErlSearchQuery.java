package org.erlide.ui.search;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.runtime.backend.ErlideBackend;

import erlang.ErlideXref;

public class ErlSearchQuery implements ISearchQuery {
	private final ErlangExternalFunctionCallRef searchRef;
	// String module;
	// String fun;
	// int arity;
	// IErlElement element;
	@SuppressWarnings("unused")
	private final List<String> scope;
	@SuppressWarnings("unused")
	private final int searchFor; // REFERENCES, DECLARATIONS or
	// ALL_OCCURRENCES
	@SuppressWarnings("unused")
	private final int limitTo;
	private ErlangSearchResult fSearchResult;
	private List<ErlangExternalFunctionCallRef> fResult;

	// public ErlSearchQuery(final String module, final String fun,
	// final int arity, final String[] scope) {
	// this.module = module;
	// this.fun = fun;
	// this.arity = arity;
	// this.scope = scope;
	// }

	// public ErlSearchQuery(final IErlElement element, final String[] scope) {
	// this.element = element;
	// this.scope = scope;
	// }

	public ErlSearchQuery(final ErlangExternalFunctionCallRef ref,
			final int searchFor, final int limitTo, final List<String> scope) {
		searchRef = ref;
		this.searchFor = searchFor;
		this.limitTo = limitTo;
		this.scope = scope;
	}

	public boolean canRerun() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canRunInBackground() {
		return false;
	}

	public String getLabel() {
		return searchRef.toString();
	}

	public ISearchResult getSearchResult() {
		if (fSearchResult == null) {
			fSearchResult = new ErlangSearchResult(this, fResult);
		}
		return fSearchResult;
	}

	public IStatus run(final IProgressMonitor monitor)
			throws OperationCanceledException {
		// FIXME här ska vi se till att alla resurser (moduler) i scope läggs
		// in... ev portionera ut lite
		final ErlideBackend backend = ErlangCore.getBackendManager()
				.getIdeBackend();
		ErlideXref.addDirs(backend, scope);
		fResult = ErlideXref.funtionUse(backend, searchRef);
		// fResult = ErlideNoparse.find(backend, searchRef);
		fSearchResult.setResult(fResult);
		final List<Match> l = new ArrayList<Match>();
		for (final ErlangExternalFunctionCallRef ref : fResult) {
			final ISourceRange pos = ref.getPos();
			final Match m = new Match(ref, pos.getOffset(), pos.getLength());
			l.add(m);
		}
		fSearchResult.addMatches(l.toArray(new Match[l.size()]));
		return Status.OK_STATUS;
	}
}
