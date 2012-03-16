package org.erlide.ui.editors.erl.outline.filters;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.ui.views.contentoutline.ContentOutline;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.editors.erl.outline.ErlangOutlinePage;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.utils.ListsUtils;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.collect.Sets.SetView;

public class OutlineFilterUtils {

    private static final String SEPARATOR = ",";

    public static boolean loadViewDefaults(
            final List<String> userDefinedPatterns,
            final Set<String> enabledFilterIDs) {
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        final boolean areUserDefinedPatternsEnabled = prefsNode.getBoolean(
                PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS_ENABLED,
                false);
        final String userDefinedPatternsString = prefsNode.get(
                PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS, "");
        userDefinedPatterns.addAll(ListsUtils.unpackList(
                userDefinedPatternsString, SEPARATOR));
        final String enabledFilterIDsString = prefsNode.get(
                PreferenceConstants.OUTLINE_ENABLED_FILTERS, "");
        enabledFilterIDs.addAll(ListsUtils.unpackList(enabledFilterIDsString,
                SEPARATOR));
        return areUserDefinedPatternsEnabled;
    }

    public static void storeViewDefaults(
            final boolean areUserDefinedPatternsEnabled,
            final List<String> userDefinedPatterns,
            final Set<String> enabledFilterIDs) {
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        prefsNode.putBoolean(
                PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS_ENABLED,
                areUserDefinedPatternsEnabled);
        prefsNode.put(PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS,
                ListsUtils.packList(userDefinedPatterns, SEPARATOR));
        prefsNode.put(PreferenceConstants.OUTLINE_ENABLED_FILTERS,
                ListsUtils.packList(enabledFilterIDs, SEPARATOR));
        try {
            prefsNode.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.error(e);
        }
    }

    public static void updateViewerFilters(final StructuredViewer viewer,
            final List<String> oldUserDefinedPatterns,
            final Set<String> oldEnabledFilterIDs,
            final List<String> userDefinedPatterns,
            final Set<String> enabledFilterIDs,
            final PatternFilter patternFilter) {
        SetView<String> intersection = Sets.intersection(oldEnabledFilterIDs,
                enabledFilterIDs);
        SetView<String> difference = Sets.difference(enabledFilterIDs,
                intersection);
        SetView<String> oldDifference = Sets.difference(oldEnabledFilterIDs,
                intersection);
        final HashSet<String> oldPatterns = Sets
                .newHashSet(oldUserDefinedPatterns);
        final HashSet<String> patterns = Sets.newHashSet(userDefinedPatterns);
        for (final String id : oldDifference) {
            final FilterDescriptor desc = FilterDescriptor
                    .getFilterDescriptor(id);
            if (desc.isClassFilter()) {
                viewer.removeFilter(desc.getViewerFilter());
            } else {
                oldPatterns.add(desc.getPattern());
            }
        }
        for (final String id : difference) {
            final FilterDescriptor desc = FilterDescriptor
                    .getFilterDescriptor(id);
            if (desc.isClassFilter()) {
                final ViewerFilter createViewerFilter = desc.getViewerFilter();
                viewer.addFilter(createViewerFilter);
            } else {
                patterns.add(desc.getPattern());
            }
        }
        intersection = Sets.intersection(oldPatterns, patterns);
        difference = Sets.difference(patterns, intersection);
        oldDifference = Sets.difference(oldPatterns, intersection);
        if (difference.isEmpty() && oldDifference.isEmpty()) {
            return;
        }
        final boolean oldPatternFilterEmpty = patternFilter.isEmpty();
        patternFilter.removePatterns(oldDifference);
        patternFilter.addPatterns(difference);
        if (oldPatternFilterEmpty != patternFilter.isEmpty()) {
            if (oldPatternFilterEmpty) {
                viewer.addFilter(patternFilter);
            } else {
                viewer.removeFilter(patternFilter);
            }
        } else {
            viewer.refresh();
        }
    }

    public static void addFilter(final String filterId, final boolean value,
            final Object activePart) {
        final FilterDescriptor desc = FilterDescriptor
                .getFilterDescriptor(filterId);
        final ViewerFilter filter = desc.getViewerFilter();
        if (filter == null) {
            return;
        }
        final ErlangOutlinePage erlangOutlinePage;
        if (activePart instanceof ErlangOutlinePage) {
            erlangOutlinePage = (ErlangOutlinePage) activePart;
        } else {
            final ContentOutline outline = (ContentOutline) activePart;
            erlangOutlinePage = (ErlangOutlinePage) outline
                    .getAdapter(ErlangOutlinePage.class);
        }
        final TreeViewer viewer = erlangOutlinePage.getTreeViewer();
        if (viewer == null) {
            return;
        }
        if (value) {
            viewer.addFilter(filter);
        } else {
            viewer.removeFilter(filter);
        }
    }

    public static void setFilters(
            final Collection<FilterDescriptor> filterDescs,
            final Object activePart) {
        final List<ViewerFilter> filters = Lists
                .newArrayListWithCapacity(filterDescs.size());
        for (final FilterDescriptor desc : filterDescs) {
            final ViewerFilter filter = desc.getViewerFilter();
            if (filter == null) {
                continue;
            }
            filters.add(filter);
        }
        final ErlangOutlinePage erlangOutlinePage;
        if (activePart instanceof ErlangOutlinePage) {
            erlangOutlinePage = (ErlangOutlinePage) activePart;
        } else {
            final ContentOutline outline = (ContentOutline) activePart;
            erlangOutlinePage = (ErlangOutlinePage) outline
                    .getAdapter(ErlangOutlinePage.class);
        }
        final TreeViewer viewer = erlangOutlinePage.getTreeViewer();
        if (viewer == null) {
            return;
        }
        viewer.setFilters(filters.toArray(new ViewerFilter[filters.size()]));
    }

}
