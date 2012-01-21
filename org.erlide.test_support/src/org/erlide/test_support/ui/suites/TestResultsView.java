package org.erlide.test_support.ui.suites;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;
import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlLogger;
import org.erlide.test_support.ui.suites.TestCaseData.FailReason;
import org.erlide.test_support.ui.suites.TestCaseData.FailStackItem;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.utils.ErlUtils;
import org.erlide.utils.TermParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class TestResultsView extends ViewPart {
    public static final String VIEW_ID = "org.erlide.test_support.views.testresults";

    private final TestEventHandler eventHandler;
    private Composite control;

    private TreeViewer treeViewer;

    private final List<TestCaseData> events;
    private Label label;

    public TestResultsView() {
        // FIXME which backend?
        eventHandler = new TestEventHandler(null, this);
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
        label.setText("");

        treeViewer = new TreeViewer(control, SWT.NONE);
        treeViewer.setSorter(new TestResultSorter());
        final Tree tree = treeViewer.getTree();
        tree.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(final MouseEvent e) {
                final Object data = tree.getSelection()[0].getData();
                if (data instanceof TestCaseData) {
                    final TestCaseData testData = (TestCaseData) data;
                    openMF(testData.getModule(), testData.getFunction());
                } else if (data instanceof FailStackItem) {
                    final FailStackItem item = (FailStackItem) data;
                    openMF(item.getModule(), item.getFunction());
                } else if (data instanceof FailReason) {
                    final FailReason reason = (FailReason) data;
                    final FailStackItem item = reason.getFirstStackItem();
                    if (item != null) {
                        openMF(item.getModule(), item.getFunction());
                    }
                }
            }
        });
        tree.setLinesVisible(true);
        tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        treeViewer.setLabelProvider(new TestResultsLabelProvider());
        treeViewer.setContentProvider(new TestResultsContentProvider());
        treeViewer.setInput(events);

        initToolbar();
    }

    private void openMF(final String module, final String function) {
        try {
            ErlModelUtils.openMF(module, function);
        } catch (final CoreException e) {
            ErlLogger.warn(e);
        }
    }

    private void initToolbar() {
        final IActionBars actionBars = getViewSite().getActionBars();
        final IMenuManager dropDownMenu = actionBars.getMenuManager();
        final IToolBarManager toolBar = actionBars.getToolBarManager();

        final Action action = new ClearTestResultsAction(treeViewer, events);
        dropDownMenu.add(action);
        toolBar.add(action);
    }

    @Override
    public void setFocus() {
        treeViewer.getTree().setFocus();
    }

    public void notifyEvent(final OtpErlangObject msg) {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                try {
                    handleEvent(msg);
                    treeViewer.refresh();
                    control.update();
                } catch (final TermParserException e) {
                    e.printStackTrace();
                } catch (final OtpErlangException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        });
    }

    private void handleEvent(final OtpErlangObject msg)
            throws TermParserException, OtpErlangException {
        final OtpErlangTuple tuple = (OtpErlangTuple) msg;
        final String tag = ((OtpErlangAtom) tuple.elementAt(0)).atomValue();
        final OtpErlangObject value = tuple.elementAt(1);

        TestCaseData test;
        if ("init".equals(tag)) {
            // value = {Dir, Suite, Case}
            label.setText("Started: " + formatTitle(value)
                    + ". Compiling files, please wait...");
            treeViewer.getTree().setCursor(
                    treeViewer.getTree().getShell().getDisplay()
                            .getSystemCursor(SWT.CURSOR_WAIT));
        } else if ("start_failed".equals(tag)) {
            // value = ?
        } else if ("log_started".equals(tag)) {
            // value = Dir
            treeViewer.getTree().setCursor(
                    treeViewer.getTree().getShell().getDisplay()
                            .getSystemCursor(SWT.CURSOR_ARROW));
        } else if ("start".equals(tag)) {
            // value = {Module, Function}
            final Bindings bindings = ErlUtils.match("{M:a,F:a}", value);
            final String mod = bindings.getAtom("M");
            final String fun = bindings.getAtom("F");
            test = findCase(mod, fun);
            test.setRunning();
        } else if ("result".equals(tag)) {
            // value = {Module, Function, Result}
            final Bindings bindings = ErlUtils.match("{M:a,F:a,R}", value);
            final String mod = bindings.getAtom("M");
            final String fun = bindings.getAtom("F");
            final OtpErlangObject result = bindings.get("R");
            test = findCase(mod, fun);
            if (result instanceof OtpErlangAtom) {
                test.setSuccesful();
                // } else {
                // final BindingsImpl bindings =
                // ErlUtils.match("{failure,{M:a,F:a},L,R}", result);
                // final OtpErlangObject locations = bindings.get("L");
                // final OtpErlangObject reason = bindings.get("R");
                // test.setFailed(reason, locations);
            }
        } else if ("fail".equals(tag)) {
            // value = {{Module, Function}, [Locations], Reason
            final Bindings bindings = ErlUtils.match("{{M:a,F:a},L,R}", value);
            final String mod = bindings.getAtom("M");
            final String fun = bindings.getAtom("F");
            final Collection<OtpErlangObject> locations = bindings.getList("L");
            final OtpErlangObject reason = bindings.get("R");
            test = findCase(mod, fun);
            test.setFailed(reason, locations);
        } else if ("skip".equals(tag)) {
            // value = {Module, Function, Comment
            final Bindings bindings = ErlUtils.match("{M:a,F:a,C}", value);
            final String mod = bindings.getAtom("M");
            final String fun = bindings.getAtom("F");
            final OtpErlangObject reason = bindings.get("C");
            test = findCase(mod, fun);
            test.setSkipped(reason);
        } else if ("done".equals(tag)) {
            // value = Module, Log, {Successful,Failed,Skipped}, [Results]}
            final Bindings bindings = ErlUtils.match("{M,L,{S:i,F:i,K:i},R}",
                    value);
            final int successful = bindings.getInt("S");
            final int failed = bindings.getInt("F");
            final int skipped = bindings.getInt("K");
            label.setText(label.getText() + " -- Done! Successful: "
                    + successful + ", Failed: " + failed + ", Skipped: "
                    + skipped);
        }
        control.redraw();
    }

    private String formatTitle(final OtpErlangObject value) {
        try {
            final Bindings b = ErlUtils.match("{D,S,C}", value);
            final String suite = b.getAtom("S");
            final String tcase = b.getAtom("C");
            if (tcase.length() == 0) {
                return "suite " + suite;
            } else {
                return "suite " + suite + "; case " + tcase;
            }
        } catch (final TermParserException e) {
        } catch (final OtpErlangException e) {
        }
        return value.toString();
    }

    private TestCaseData findCase(final String mod, final String fun) {
        for (final TestCaseData data : events) {
            if (data.getModule().equals(mod) && data.getFunction().equals(fun)) {
                return data;
            }
        }
        final TestCaseData data = new TestCaseData(mod, fun);
        events.add(data);
        return data;
    }

    public void clearEvents() {
        events.clear();
        treeViewer.refresh();
    }

    public void setMessage(final String string) {
        label.setText(string);
        label.update();
    }

}
