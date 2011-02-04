package org.erlide.test_support.ui.suites;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ResultsView extends ViewPart {
    public static final String VIEW_ID = "org.erlide.test_support.views.testresults";

    private final TestEventHandler eventHandler;
    private Composite control;

    private TreeViewer treeViewer;

    private final List<OtpErlangObject> events;

    public ResultsView() {
        eventHandler = new TestEventHandler(this);
        events = Lists.newArrayList();
    }

    public TestEventHandler getEventHandler() {
        return eventHandler;
    }

    @Override
    public void createPartControl(final Composite parent) {
        control = new Composite(parent, SWT.NONE);
        control.setLayout(new GridLayout(1, false));

        treeViewer = new TreeViewer(control, SWT.NONE);
        final Tree tree = treeViewer.getTree();
        tree.setLinesVisible(true);
        tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        treeViewer.setLabelProvider(new TestResultsLabelProvider());
        treeViewer.setContentProvider(new TestResultsContentProvider());
        treeViewer.setInput(events);

        initToolbar();
    }

    private void initToolbar() {
        final Action action = new ClearResultsAction(treeViewer, events);
        final IActionBars actionBars = getViewSite().getActionBars();
        final IMenuManager dropDownMenu = actionBars.getMenuManager();
        final IToolBarManager toolBar = actionBars.getToolBarManager();
        dropDownMenu.add(action);
        toolBar.add(action);
    }

    @Override
    public void setFocus() {
        treeViewer.getTree().setFocus();
    }

    public void notifyEvent(final OtpErlangObject msg) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                events.add(msg);
                treeViewer.refresh();
            }
        });
    }

}
