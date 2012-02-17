package org.erlide.ui.editors.erl.outline;

import java.util.List;
import java.util.Set;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.views.contentoutline.ContentOutline;
import org.erlide.ui.editors.erl.outline.filters.CustomOutlineFiltersDialog;
import org.erlide.ui.editors.erl.outline.filters.OutlineFilterUtils;
import org.erlide.ui.editors.erl.outline.filters.PatternFilter;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ShowCustomOutlineFiltersDialogHandler extends AbstractHandler
        implements IHandler {

    public ShowCustomOutlineFiltersDialogHandler() {
        super();
    }

    @Override
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
        final boolean oldAreUserDefinedPatternsEnabled = OutlineFilterUtils
                .loadViewDefaults(oldUserDefinedPatterns, oldEnabledFilterIDs);
        final CustomOutlineFiltersDialog dialog = new CustomOutlineFiltersDialog(
                shell, targetId, oldAreUserDefinedPatternsEnabled,
                oldUserDefinedPatterns, oldEnabledFilterIDs);
        if (!oldAreUserDefinedPatternsEnabled) {
            oldUserDefinedPatterns = Lists.newArrayList();
        }
        if (dialog.open() == Window.OK) {
            final boolean areUserDefinedPatternsEnabled = dialog
                    .areUserDefinedPatternsEnabled();
            List<String> userDefinedPatterns = dialog.getUserDefinedPatterns();
            final Set<String> enabledFilterIDs = dialog.getEnabledFilterIds();
            // TODO should we support filter LRU history (as in JDT)?
            // setRecentlyChangedFilters(dialog.getFilterDescriptorChangeHistory());
            OutlineFilterUtils.storeViewDefaults(areUserDefinedPatternsEnabled,
                    userDefinedPatterns, enabledFilterIDs);
            if (!areUserDefinedPatternsEnabled) {
                userDefinedPatterns = Lists.newArrayList();
            }
            OutlineFilterUtils.updateViewerFilters(viewer,
                    oldUserDefinedPatterns, oldEnabledFilterIDs,
                    userDefinedPatterns, enabledFilterIDs, patternFilter);
        }

        // TODO Auto-generated method stub
        return null;
    }
}
