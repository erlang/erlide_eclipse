package org.erlide.ui.search;

import static erlang.ErlideSearchServer.includeUse;

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
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.core.search.ErlangIncludeRef;
import org.erlide.core.search.ErlangMacroRef;
import org.erlide.core.search.ErlangRecordRef;
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
	private final int searchFor; // REFERENCES, DECLARATIONS or
	// ALL_OCCURRENCES
	private final int limitTo;
	private ErlangSearchResult fSearchResult;
	private List<ModuleLineFunctionArityRef> fResult;

	private String stateDir = null;

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

	public ErlSearchQuery(final ErlangElementRef ref, final int searchFor,
			final int limitTo, final List<IResource> scope) {
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
		// ErlideSearchServer.addModules(backend, scope);
		if (searchRef instanceof ErlangExternalFunctionCallRef) {
			final ErlangExternalFunctionCallRef r = (ErlangExternalFunctionCallRef) searchRef;
			fResult = ErlideSearchServer.functionUse(backend, r.getModule(), r
					.getFunction(), r.getArity(), scope, getStateDir());
		} else if (searchRef instanceof ErlangMacroRef) {
			final ErlangMacroRef r = (ErlangMacroRef) searchRef;
			fResult = ErlideSearchServer.macroOrRecordUse(backend, "macro", r
					.getMacro(), scope, getStateDir());
		} else if (searchRef instanceof ErlangRecordRef) {
			final ErlangRecordRef r = (ErlangRecordRef) searchRef;
			fResult = ErlideSearchServer.macroOrRecordUse(backend, "record", r
					.getRecord(), scope, getStateDir());
		} else if (searchRef instanceof ErlangIncludeRef) {
			final ErlangIncludeRef r = (ErlangIncludeRef) searchRef;
			fResult = includeUse(backend, r.getFilename(), scope, getStateDir());
		}
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
		if (stateDir == null) {
			stateDir = ErlangPlugin.getDefault().getStateLocation().toString();
		}
		return stateDir;
	}
}
