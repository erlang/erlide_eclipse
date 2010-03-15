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
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.ResourceUtil;
import org.erlide.ui.util.ErlModelUtils;

public class ErlangSearchResult extends AbstractTextSearchResult implements
		IEditorMatchAdapter, IFileMatchAdapter {

	private List<ErlangSearchElement> result;
	private final ErlSearchQuery query;

	@Override
	public void removeAll() {
		result = new ArrayList<ErlangSearchElement>();
		super.removeAll();
	}

	@Override
	public void removeMatch(final Match match) {
		result.remove(match.getElement());
		super.removeMatch(match);
	}

	@Override
	public void removeMatches(final Match[] matches) {
		for (final Match match : matches) {
			result.remove(match.getElement());
		}
		super.removeMatches(matches);
	}

	public ErlangSearchResult(final ErlSearchQuery query) {
		this.query = query;
		result = new ArrayList<ErlangSearchElement>();
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
		final int matchCount = getMatchCount();
		final String occurences = matchCount == 1 ? "occurence."
				: "occurences.";
		return query.getLabel() + " - " + matchCount + " " + occurences;
	}

	public ISearchQuery getQuery() {
		return query;
	}

	public String getTooltip() {
		// TODO Auto-generated method stub
		return null;
	}

	public List<ErlangSearchElement> getResult() {
		return result;
	}

	public synchronized void setResult(final List<ErlangSearchElement> result) {
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
		final ErlangSearchElement ese = (ErlangSearchElement) match
				.getElement();
		final IFile file = ResourceUtil
				.getFileFromLocation(ese.getModuleName());
		if (editor instanceof ITextEditor) {
			final ITextEditor textEditor = (ITextEditor) editor;
			return ErlModelUtils.getModule(textEditor).getResource().equals(
					file);
		}
		return false;
	}

	public Match[] computeContainedMatches(
			final AbstractTextSearchResult aResult, final IFile file) {
		final ErlangSearchResult esr = (ErlangSearchResult) aResult;
		final List<Match> l = new ArrayList<Match>();
		final List<ErlangSearchElement> eses = esr.getResult();
		String name = file.getName();
		if (eses == null || !ErlideUtil.hasModuleExtension(name)) {
			return NO_MATCHES;
		}
		name = ErlideUtil.withoutExtension(name);
		for (final ErlangSearchElement ese : eses) {
			if (ese.getModuleName().equals(name)) {
				final Match[] matches = getMatches(ese);
				for (final Match match : matches) {
					l.add(match);
				}
			}
		}
		return l.toArray(new Match[l.size()]);
	}

	public IFile getFile(final Object element) {
		if (element instanceof ErlangSearchElement) {
			final ErlangSearchElement ese = (ErlangSearchElement) element;
			return ResourceUtil.getFileFromLocation(ese.getModuleName());
		}
		return null;
	}
}
