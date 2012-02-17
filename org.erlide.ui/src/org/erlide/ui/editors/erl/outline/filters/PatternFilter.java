package org.erlide.ui.editors.erl.outline.filters;

import java.util.Collection;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.ui.dialogs.SearchPattern;
import org.erlide.core.model.root.IErlElement;
import org.erlide.ui.editors.erl.outline.ErlangLabelProvider;

import com.google.common.collect.Maps;

public class PatternFilter extends ViewerFilter {

    private final Map<String, SearchPattern> fPatterns = Maps.newHashMap();

    @Override
    public boolean select(final Viewer viewer, final Object parentElement,
            final Object element) {
        for (final SearchPattern pattern : fPatterns.values()) {
            if (pattern.matches(getName(element))) {
                return false;
            }
        }
        return true;
    }

    private String getName(final Object element) {
        if (element instanceof IErlElement) {
            return ErlangLabelProvider.getLabelString(element);
        } else if (element instanceof IResource) {
            final IResource resource = (IResource) element;
            return resource.getName();
        }
        return element.toString();
    }

    public void removePatterns(final Collection<String> patterns) {
        for (final String pattern : patterns) {
            fPatterns.remove(pattern);
        }
    }

    public void addPatterns(final Collection<String> patterns) {
        for (final String pattern : patterns) {
            final SearchPattern searchPattern = new SearchPattern(
                    SearchPattern.RULE_PATTERN_MATCH);
            searchPattern.setPattern(pattern);
            fPatterns.put(pattern, searchPattern);
        }
    }

    public boolean isEmpty() {
        return fPatterns.isEmpty();
    }

}
