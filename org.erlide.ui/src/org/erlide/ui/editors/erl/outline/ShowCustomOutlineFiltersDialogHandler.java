package org.erlide.ui.editors.erl.outline;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.views.contentoutline.ContentOutline;
import org.erlide.core.common.CommonUtils;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.prefs.PreferenceConstants;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.collect.Sets.SetView;

public class ShowCustomOutlineFiltersDialogHandler extends AbstractHandler
        implements IHandler {

    public ShowCustomOutlineFiltersDialogHandler() {
        super();
        // TODO Auto-generated constructor stub
    }

    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final Object activePart = HandlerUtil.getVariable(event, "activePart");
        final String targetId;
        final Shell shell;
        PatternFilter patternFilter;
        StructuredViewer viewer;
        if (activePart instanceof ContentOutline) {
            final ContentOutline outline = (ContentOutline) activePart;
            final ErlangOutlinePage erlangOutlinePage = (ErlangOutlinePage) outline
                    .getAdapter(ErlangOutlinePage.class);
            shell = outline.getSite().getShell();
            targetId = "org.eclipse.ui.views.ContentOutline";
            patternFilter = erlangOutlinePage.getPatternFilter();
            viewer = erlangOutlinePage.getTreeViewer();
        } else if (activePart instanceof CommonNavigator) {
            final CommonNavigator commonNavigator = (CommonNavigator) activePart;
            targetId = "commonNavigator";
            shell = commonNavigator.getSite().getShell();
            patternFilter = null;
            viewer = null;
        } else {
            return null;
        }
        List<String> oldUserDefinedPatterns = Lists.newArrayList();
        final Set<String> oldEnabledFilterIDs = Sets.newHashSet();
        final boolean oldAreUserDefinedPatternsEnabled = loadViewDefaults(
                oldUserDefinedPatterns, oldEnabledFilterIDs);
        final CustomOutlineFiltersDialog dialog = new CustomOutlineFiltersDialog(
                shell, targetId, oldAreUserDefinedPatternsEnabled,
                oldUserDefinedPatterns, oldEnabledFilterIDs);
        if (oldAreUserDefinedPatternsEnabled) {
            oldUserDefinedPatterns = Lists.newArrayList();
        }
        if (dialog.open() == Window.OK) {
            final boolean areUserDefinedPatternsEnabled = dialog
                    .areUserDefinedPatternsEnabled();
            List<String> userDefinedPatterns = dialog.getUserDefinedPatterns();
            final Set<String> enabledFilterIDs = dialog.getEnabledFilterIds();
            // TODO should we support filter LRU history (as in JDT)?
            // setRecentlyChangedFilters(dialog.getFilterDescriptorChangeHistory());
            storeViewDefaults(areUserDefinedPatternsEnabled,
                    userDefinedPatterns, enabledFilterIDs);
            if (!areUserDefinedPatternsEnabled) {
                userDefinedPatterns = Lists.newArrayList();
            }
            updateViewerFilters(viewer, oldUserDefinedPatterns,
                    oldEnabledFilterIDs, userDefinedPatterns, enabledFilterIDs,
                    patternFilter);
        }

        // TODO Auto-generated method stub
        return null;
    }

    private static final String SEPARATOR = ",";

    private boolean loadViewDefaults(final List<String> oldUserDefinedPatterns,
            final Set<String> oldEnabledFilterIDs) {
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        final boolean areUserDefinedPatternsEnabled = prefsNode.getBoolean(
                PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS_ENABLED,
                false);
        final String userDefinedPatterns = prefsNode.get(
                PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS, "");
        oldUserDefinedPatterns.addAll(CommonUtils.unpackList(
                userDefinedPatterns, SEPARATOR));
        final String enabledFilterIDs = prefsNode.get(
                PreferenceConstants.OUTLINE_ENABLED_FILTERS, "");
        oldEnabledFilterIDs.addAll(CommonUtils.unpackList(enabledFilterIDs,
                SEPARATOR));
        return areUserDefinedPatternsEnabled;
    }

    private void storeViewDefaults(final boolean areUserDefinedPatternsEnabled,
            final List<String> userDefinedPatterns,
            final Set<String> enabledFilterIDs) {
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        prefsNode.putBoolean(
                PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS_ENABLED,
                areUserDefinedPatternsEnabled);
        prefsNode.put(PreferenceConstants.OUTLINE_CUSTOM_PATTERN_FILTERS,
                CommonUtils.packList(userDefinedPatterns, SEPARATOR));
        prefsNode.put(PreferenceConstants.OUTLINE_ENABLED_FILTERS,
                CommonUtils.packList(enabledFilterIDs, SEPARATOR));
        try {
            prefsNode.flush();
        } catch (final BackingStoreException e) {
            ErlLogger.error(e);
        }
    }

    private void updateViewerFilters(final StructuredViewer viewer,
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
}
