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
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.runtime.backend.ErlideBackend;

import erlang.ErlideSearchServer;

public class ErlSearchQuery implements ISearchQuery {
	private final ErlangExternalFunctionCallRef searchRef;
	// String module;
	// String fun;
	// int arity;
	// IErlElement element;
	private final List<String> scope;
	private final int searchFor; // REFERENCES, DECLARATIONS or
	// ALL_OCCURRENCES
	private final int limitTo;
	private ErlangSearchResult fSearchResult;
	private List<ModuleLineFunctionArityRef> fResult;

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
		// FIXME här ska vi se till att alla resurser (moduler) i scope läggs
		// in... ev portionera ut lite
		// TODO should be setScope
		ErlideSearchServer.addModules(backend, scope);
		fResult = ErlideSearchServer.functionUse(backend,
				searchRef.getModule(), searchRef.getFunction(), searchRef
						.getArity());
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
}
