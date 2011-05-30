package org.erlide.test_support.ui.suites;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.TreeViewer;
import org.erlide.ui.ErlideImage;

public class ClearTestResultsAction extends Action {
    private final List<TestCaseData> events;
    private final TreeViewer treeViewer;

    public ClearTestResultsAction(final TreeViewer treeViewer,
            final List<TestCaseData> events) {
        super("Clear results");
        this.events = events;
        this.treeViewer = treeViewer;
        setImageDescriptor(ErlideImage.CLEAR.getDescriptor());
    }

    @Override
    public void run() {
        events.clear();
        treeViewer.refresh();
    }
}
