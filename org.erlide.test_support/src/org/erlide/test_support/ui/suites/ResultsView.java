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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;
import org.erlide.jinterface.util.Bindings;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ResultsView extends ViewPart {
    public static final String VIEW_ID = "org.erlide.test_support.views.testresults";

    private final TestEventHandler eventHandler;
    private Composite control;

    private TreeViewer treeViewer;

    private final List<TestCaseData> events;
    private Label label;

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

        label = new Label(control, SWT.NONE);
        label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                1));
        label.setText("...");

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
                try {
                    handleEvent(msg);
                    treeViewer.refresh();
                    control.update();
                } catch (final ParserException e) {
                    e.printStackTrace();
                } catch (final OtpErlangException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        });
    }

    private void handleEvent(final OtpErlangObject msg) throws ParserException,
            OtpErlangException {
        final OtpErlangTuple tuple = (OtpErlangTuple) msg;
        final String tag = ((OtpErlangAtom) tuple.elementAt(0)).atomValue();
        final OtpErlangObject value = tuple.elementAt(1);

        System.out.println(tag);
        System.out.println(value);
        System.out.println("---");

        TestCaseData test;
        if ("tc_init".equals(tag)) {
            // value = {Dir, Suite, Case}
            final String title = value.toString();
            label.setText(">>> ");
        } else if ("start_failed".equals(tag)) {
            // value = ?
        } else if ("log_started".equals(tag)) {
            // value = Dir
        } else if ("start".equals(tag)) {
            // value = {Module, Function}
            final Bindings bindings = ErlUtils.match("{M:a,F:a}", value);
            final String mod = bindings.getAtom("M");
            final String fun = bindings.getAtom("F");
            final TestCaseData data = new TestCaseData(mod, fun);
            events.add(data);
            data.setRunning();
        } else if ("result".equals(tag)) {
            // value = {{Module, Function}, Result}
            final Bindings bindings = ErlUtils.match("{M:a,F:a,_}", value);
            final String mod = bindings.getAtom("M");
            final String fun = bindings.getAtom("F");
            test = findCase(mod, fun);
            test.setSuccesful();
        } else if ("fail".equals(tag)) {
            // value = {{Module, Function}, [Locations], Reason
            final Bindings bindings = ErlUtils.match("{{M:a,F:a},L,R}", value);
            final String mod = bindings.getAtom("M");
            final String fun = bindings.getAtom("F");
            final OtpErlangObject locations = bindings.get("L");
            final OtpErlangObject reason = bindings.get("R");
            test = findCase(mod, fun);
            test.setFailed(reason, locations);
        } else if ("done".equals(tag)) {
            // value = Modle, Log, {Successful,Failed,Skipped}, [Results]}
            final Bindings bindings = ErlUtils.match("{M,L,{S:i,F:i,K:i},R}",
                    value);
            final int successful = bindings.getInt("S");
            final int failed = bindings.getInt("F");
            final int skipped = bindings.getInt("K");
            label.setText(label.getText() + " -- Done! Successful: "
                    + successful + ", Failed: " + failed + ", Skipped: "
                    + skipped);
        }
    }

    private TestCaseData findCase(final String mod, final String fun) {
        for (final TestCaseData data : events) {
            if (data.getModule().equals(mod) && data.getFunction().equals(fun)) {
                return data;
            }
        }
        return null;
    }

    public void clearEvents() {
        events.clear();
        treeViewer.refresh();
    }

}
