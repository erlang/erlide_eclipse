/**
 *
 */
package org.erlide.ui.internal.search;

import java.text.MessageFormat;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.swt.graphics.Image;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.core.model.util.ResourceUtil;
import org.erlide.ui.editors.erl.outline.ErlangElementImageProvider;

public class SearchResultLabelProvider extends LabelProvider implements
        IStyledLabelProvider {

    public static final int SHOW_LABEL = 1;
    public static final int SHOW_LABEL_PATH = 2;
    public static final int SHOW_PATH_LABEL = 3;

    private final ErlangElementImageProvider fImageProvider;
    private final AbstractTextSearchViewPage fPage;

    private int fOrder;
    private final boolean fIsInTree;

    // private final String[] fArgs = new String[2];

    public SearchResultLabelProvider(final AbstractTextSearchViewPage page,
            final int order, final boolean isInTree) {
        fIsInTree = isInTree;
        fImageProvider = new ErlangElementImageProvider();
        fOrder = order;
        fPage = page;
    }

    public void setOrder(final int orderFlag) {
        fOrder = orderFlag;
    }

    public int getOrder() {
        return fOrder;
    }

    private StyledString getPathText(final Object element) {
        String location;
        if (element instanceof String) {
            final String s = (String) element;
            location = new Path(s).removeLastSegments(1).toPortableString();
        } else if (element instanceof ErlangSearchElement) {
            final ErlangSearchElement ese = (ErlangSearchElement) element;
            location = ese.getModuleName();
        } else {
            location = "";
        }
        final IFile file = ResourceUtil.getFileFromLocation(location);
        final String s;
        if (file != null) {
            s = file.getFullPath().toPortableString();
        } else {
            s = location;
        }
        return new StyledString(s, StyledString.QUALIFIER_STYLER);
    }

    private String getElementText(final Object element) {
        if (element instanceof String) { // Module
            final String s = (String) element;
            return new Path(s).lastSegment();
        } else if (element instanceof ErlangSearchElement) {
            final ErlangSearchElement ese = (ErlangSearchElement) element;
            return searchElementToString(ese);
        } else if (element instanceof ErlangFunction) {
            final ErlangFunction f = (ErlangFunction) element;
            return f.getNameWithArity();
        }
        return "";
    }

    private static String searchElementFunctionToString(
            final ErlangSearchElement ese) {
        final String a = ese.getArguments();
        if (ese.isSubClause()) {
            return ese.getName() + a;
        } else {
            final String nameWithArity = ese.getName() + "/" + ese.getArity();
            if (a != null) {
                return nameWithArity + "  " + a;
            } else {
                return nameWithArity;
            }
        }
    }

    private String searchElementToString(final ErlangSearchElement ese) {
        switch (ese.getKind()) {
        case ATTRIBUTE:
            return ese.getName();
        case CLAUSE:
        case FUNCTION:
            return searchElementFunctionToString(ese);
        case RECORD_DEF:
            return "record_definition: " + ese.getName();
        case MACRO_DEF:
            return "macro_definition: " + ese.getName();
        default:
            return ese.getName();
        }
    }

    private StyledString getMatchCountText(final Object element) {
        int matchCount = 0;
        final AbstractTextSearchResult result = fPage.getInput();
        if (result != null) {
            matchCount = fPage.getDisplayedMatchCount(element);
        }
        if (matchCount > 1) {
            final String countInfo = MessageFormat.format("({0} matches)",
                    Integer.valueOf(matchCount));
            return new StyledString(countInfo, StyledString.COUNTER_STYLER);
        }
        return new StyledString();
    }

    @Override
    public Image getImage(final Object element) {
        // module - String
        // function - ErlangFunction
        // clause - ClauseHead
        // occurrence - ModuleLineFunctionArityRef
        Kind kind = Kind.ERROR;
        if (element instanceof String) {
            kind = Kind.MODULE;
        } else if (element instanceof ErlangSearchElement) {
            final ErlangSearchElement ese = (ErlangSearchElement) element;
            kind = ese.getKind();
            if (kind == Kind.FUNCTION) {
                final IErlModule module = ese.getModule();
                try {
                    module.open(null);
                } catch (final ErlModelException e) {
                }
                final IErlFunction function = module
                        .findFunction(new ErlangFunction(ese.getName(), ese
                                .getArity()));
                if (function != null && function.isExported()) {
                    kind = Kind.EXPORTFUNCTION;
                }
            }
        } else if (element instanceof ErlangFunction) {
            kind = Kind.FUNCTION;
        }
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

    @Override
    public StyledString getStyledText(final Object element) {
        final StyledString result = new StyledString();
        if (fOrder == SHOW_LABEL_PATH || element instanceof String
                && isInTree()) {
            result.append(getElementText(element));
            result.append(' ');
            result.append(getMatchCountText(element));
            result.append(" - ", StyledString.QUALIFIER_STYLER);
            result.append(getPathText(element));
        } else if (fOrder == SHOW_LABEL) {
            result.append(getElementText(element));
            result.append(' ');
            result.append(getMatchCountText(element));
        } else { // SHOW_PATH_LABEL
            result.append(getElementText(element));
            result.append(' ');
            result.append(getMatchCountText(element));
            result.append(" - ", StyledString.QUALIFIER_STYLER);
            result.append(getPathText(element));
        }
        return result;
    }

    private boolean isInTree() {
        return fIsInTree;
    }

}
