package org.erlide.ui.search;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.IEditorMatchAdapter;
import org.eclipse.search.ui.text.IFileMatchAdapter;
import org.eclipse.search.ui.text.Match;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.search.ErlangExternalFunctionCallRef;
import org.erlide.ui.util.ErlModelUtils;

public class ErlangSearchResult extends AbstractTextSearchResult implements
		IEditorMatchAdapter, IFileMatchAdapter {

	private List<ErlangExternalFunctionCallRef> result;
	private final ErlSearchQuery query;

	public ErlangSearchResult(final ErlSearchQuery query,
			final List<ErlangExternalFunctionCallRef> result) {
		this.query = query;
		this.result = result;
	}

	@Override
	public IEditorMatchAdapter getEditorMatchAdapter() {
		return this;
	}

	@Override
	public IFileMatchAdapter getFileMatchAdapter() {
		return this;
	}

	public ImageDescriptor getImageDescriptor() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getLabel() {
		return query.getLabel() + " - " + getMatchCount() + " occurences.";
	}

	public ISearchQuery getQuery() {
		return query;
	}

	public String getTooltip() {
		// TODO Auto-generated method stub
		return null;
	}

	public List<ErlangExternalFunctionCallRef> getResult() {
		return result;
	}

	public synchronized void setResult(
			final List<ErlangExternalFunctionCallRef> result) {
		this.result = result;
	}

	private static final Match[] NO_MATCHES = new Match[0];

	public Match[] computeContainedMatches(
			final AbstractTextSearchResult aResult, final IEditorPart editor) {
		// TODO: copied from JavaSearchResult:
		final IEditorInput editorInput = editor.getEditorInput();
		if (editorInput instanceof IFileEditorInput) {
			final IFileEditorInput fileEditorInput = (IFileEditorInput) editorInput;
			return computeContainedMatches(aResult, fileEditorInput.getFile());
		}
		return NO_MATCHES;
	}

	public boolean isShownInEditor(final Match match, final IEditorPart editor) {
		final ErlangExternalFunctionCallRef ref = (ErlangExternalFunctionCallRef) match
				.getElement();
		final IErlElement e = ref.getParent();
		if (editor instanceof ITextEditor) {
			final ITextEditor textEditor = (ITextEditor) editor;
			final IErlModule mod = SearchUtil.getModule(e);
			return mod == ErlModelUtils.getModule(textEditor);
		}
		return false;
	}

	public Match[] computeContainedMatches(
			final AbstractTextSearchResult aResult, final IFile file) {
		final ErlangSearchResult esr = (ErlangSearchResult) aResult;
		final List<Match> l = new ArrayList<Match>();
		final List<ErlangExternalFunctionCallRef> refs = esr.getResult();
		if (refs == null) {
			return NO_MATCHES;
		}
		for (final ErlangExternalFunctionCallRef ref : refs) {
			final IErlElement e = ref.getParent();
			final IErlModule mod = SearchUtil.getModule(e);
			if (mod.getResource() == file) {
				final ISourceRange pos = ref.getPos();
				final Match m = new Match(ref, pos.getOffset(), pos.getLength());
				l.add(m);
			}
		}
		return l.toArray(new Match[l.size()]);
	}

	public IFile getFile(final Object element) {
		if (element instanceof IErlElement) {
			final IErlElement e = (IErlElement) element;
			final IErlModule mod = SearchUtil.getModule(e);
			if (mod != null) {
				return (IFile) mod.getResource();
			}
		} else if (element instanceof ErlangExternalFunctionCallRef) {
			final ErlangExternalFunctionCallRef ref = (ErlangExternalFunctionCallRef) element;
			return getFile(ref.getParent());
		}
		return null;
	}
}
