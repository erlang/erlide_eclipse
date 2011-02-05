package org.erlide.test_support.ui.suites;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.internal.console.ConsolePluginImages;
import org.eclipse.ui.internal.console.IInternalConsoleConstants;

public class ClearResultsAction extends Action {
    private final List<TestCaseData> events;
    private final TreeViewer treeViewer;

    public ClearResultsAction(final TreeViewer treeViewer,
            final List<TestCaseData> events) {
        super("Clear results");
        this.events = events;
        this.treeViewer = treeViewer;
        setImageDescriptor(ConsolePluginImages
                .getImageDescriptor(IInternalConsoleConstants.IMG_ELCL_CLEAR));
    }

    @Override
    public void run() {
        events.clear();
        treeViewer.refresh();
    }
}
