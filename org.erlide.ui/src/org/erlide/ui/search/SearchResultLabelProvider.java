/**
 * 
 */
package org.erlide.ui.search;

import java.text.MessageFormat;

import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.swt.graphics.Image;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.util.ErlangFunction;
import org.erlide.ui.editors.erl.outline.ErlangElementImageProvider;

public class SearchResultLabelProvider extends LabelProvider {

	public static final int SHOW_LABEL = 1;
	public static final int SHOW_LABEL_PATH = 2;
	public static final int SHOW_PATH_LABEL = 3;
	public static final int SHOW_PATH = 4;

	private final ErlangElementImageProvider fImageProvider;
	private final AbstractTextSearchViewPage fPage;

	private int fOrder;

	// private final String[] fArgs = new String[2];

	public SearchResultLabelProvider(final AbstractTextSearchViewPage page,
			final int orderFlag) {
		fImageProvider = new ErlangElementImageProvider();
		fOrder = orderFlag;
		fPage = page;
	}

	public void setOrder(final int orderFlag) {
		fOrder = orderFlag;
	}

	public int getOrder() {
		return fOrder;
	}

	@Override
	public String getText(final Object element) {
		final String text;
		if (element instanceof String) { // Module
			final String s = (String) element;
			text = new Path(s).lastSegment();
		} else if (element instanceof ErlangSearchElement) {
			final ErlangSearchElement ese = (ErlangSearchElement) element;
			final String arguments = ese.getArguments();
			final ErlangFunction function = ese.getFunction();
			if (ese.isSubClause()) {
				text = function.name + arguments;
			} else {
				final String nameWithArity = function.getNameWithArity();
				if (arguments != null) {
					text = nameWithArity + "  " + arguments;
				} else {
					text = nameWithArity;
				}
			}
		} else if (element instanceof ErlangFunction) {
			final ErlangFunction f = (ErlangFunction) element;
			text = f.getNameWithArity();
		} else {
			text = null;
		}
		int matchCount = 0;
		final AbstractTextSearchResult result = fPage.getInput();
		if (result != null) {
			matchCount = fPage.getDisplayedMatchCount(element);
		}
		if (matchCount == 0) {
			return text;
		} else if (matchCount == 1) {
			return MessageFormat.format("{0} 1 match", text);
		}
		final String format = "{0} ({1} matches)";
		return MessageFormat.format(format, text, Integer.valueOf(matchCount));
	}

	@Override
	public Image getImage(final Object element) {
		// module - String
		// function - ErlangFunction
		// clause - ClauseHead
		// occurence - ModuleLineFunctionArityRef
		Kind kind = Kind.ERROR;
		if (element instanceof String) {
			kind = Kind.MODULE;
		} else if (element instanceof ErlangSearchElement) {
			final ErlangSearchElement ese = (ErlangSearchElement) element;
			if (ese.isSubClause()) {
				kind = Kind.FUNCTION;
			} else {
				kind = Kind.CLAUSE;
			}
		} else if (element instanceof ErlangFunction) {
			kind = Kind.FUNCTION;
		}
		// if (element instanceof ModuleLineFunctionArityRef) {
		// final ModuleLineFunctionArityRef mlfar =
		// (ModuleLineFunctionArityRef) element;
		// ErlLogger.debug("fixa");// TODO fixa
		// e = null;
		// } else {
		// e = element;
		// }
		return fImageProvider.getImageLabel(ErlangElementImageProvider
				.getImageDescriptionFromKind(kind));
	}

	@Override
	public void dispose() {
		super.dispose();
	}

	@Override
	public boolean isLabelProperty(final Object element, final String property) {
		return super.isLabelProperty(element, property);
		// return fLabelProvider.isLabelProperty(element, property);
	}

	@Override
	public void removeListener(final ILabelProviderListener listener) {
		super.removeListener(listener);
		// fLabelProvider.removeListener(listener);
	}

	@Override
	public void addListener(final ILabelProviderListener listener) {
		super.addListener(listener);
		// fLabelProvider.addListener(listener);
	}
}